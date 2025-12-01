/*
  big_calculator.c
  A large, feature-rich scientific calculator in C.
  - Supports infix expressions, functions, constants.
  - Implements shunting-yard to convert to RPN and then evaluates.
  - Single-file. Compile with: gcc big_calculator.c -o big_calc -lm

  Notes:
  - Uses math.h; link with -lm.
  - Modest protections for invalid input; not bulletproof but robust.
  - Angle mode is default RADIANS; use "mode deg" to switch to degrees.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#if defined(_MSC_VER)
#define _USE_MATH_DEFINES
#endif
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_E
#define M_E 2.71828182845904523536
#endif

#include <limits.h>
#include <errno.h>

#define MAX_TOKEN_LEN 128
#define MAX_TOKENS 4096
#define HISTORY_SIZE 256
#define STACK_INIT_CAP 256

typedef enum { TOKEN_NUMBER, TOKEN_OPERATOR, TOKEN_FUNCTION, TOKEN_PAREN_LEFT, TOKEN_PAREN_RIGHT, TOKEN_COMMA, TOKEN_CONSTANT, TOKEN_IDENTIFIER } TokenType;

typedef struct {
    TokenType type;
    char str[MAX_TOKEN_LEN];
    double value; // for number tokens or evaluated constants
} Token;

typedef struct {
    Token *data;
    int size;
    int capacity;
} TokenArray;

typedef enum { MODE_RAD, MODE_DEG } AngleMode;

static AngleMode angle_mode = MODE_RAD;
static double memory_slot = 0.0;

void token_array_init(TokenArray *arr) {
    arr->capacity = 256;
    arr->size = 0;
    arr->data = (Token*)malloc(sizeof(Token) * arr->capacity);
    if (!arr->data) { perror("malloc"); exit(1); }
}
void token_array_push(TokenArray *arr, Token t) {
    if (arr->size >= arr->capacity) {
        arr->capacity *= 2;
        arr->data = (Token*)realloc(arr->data, sizeof(Token) * arr->capacity);
        if (!arr->data) { perror("realloc"); exit(1); }
    }
    arr->data[arr->size++] = t;
}
void token_array_free(TokenArray *arr) {
    free(arr->data);
    arr->data = NULL;
    arr->size = arr->capacity = 0;
}

/* ---------- Utility helpers ---------- */

int str_eq_nocase(const char *a, const char *b) {
    while (*a && *b) {
        if (tolower((unsigned char)*a) != tolower((unsigned char)*b)) return 0;
        a++; b++;
    }
    return *a == 0 && *b == 0;
}

void trim_trailing_newline(char *s) {
    size_t n = strlen(s);
    if (n == 0) return;
    if (s[n-1] == '\n') s[n-1] = '\0';
}

/* ---------- Functions & operators metadata ---------- */

int is_function_name(const char *s) {
    const char *funcs[] = {
        "sin","cos","tan","asin","acos","atan",
        "sinh","cosh","tanh",
        "sqrt","cbrt","ln","log","exp","pow",
        "abs","floor","ceil","fact","nCr","nPr",
        "gcd","lcm"
    };
    for (size_t i = 0; i < sizeof(funcs)/sizeof(funcs[0]); ++i)
        if (str_eq_nocase(s, funcs[i])) return 1;
    return 0;
}

int is_constant_name(const char *s) {
    if (str_eq_nocase(s, "pi")) return 1;
    if (str_eq_nocase(s, "e")) return 1;
    if (str_eq_nocase(s, "M")) return 1; // memory recall identifier
    return 0;
}

int is_identifier_char(char c) {
    return isalpha((unsigned char)c) || c == '_' || c == '$';
}

/* precedence: higher number = higher precedence */
int op_precedence(char op) {
    switch (op) {
        case '+': case '-': return 2;
        case '*': case '/': case '%': return 3;
        case '^': return 4;
        default: return 0;
    }
}
int op_right_associative(char op) {
    return (op == '^');
}

int is_operator_char(char c) {
    return c == '+' || c == '-' || c == '*' || c == '/' || c == '^' || c == '%';
}

/* ---------- Tokenizer ---------- */

void push_number_token(TokenArray *arr, const char *s, size_t len) {
    Token t;
    t.type = TOKEN_NUMBER;
    if (len >= MAX_TOKEN_LEN) len = MAX_TOKEN_LEN-1;
    strncpy(t.str, s, len);
    t.str[len] = '\0';
    errno = 0;
    t.value = strtod(t.str, NULL);
    token_array_push(arr, t);
}

void push_operator_token(TokenArray *arr, char op) {
    Token t;
    t.type = TOKEN_OPERATOR;
    t.str[0] = op;
    t.str[1] = '\0';
    t.value = 0.0;
    token_array_push(arr, t);
}

void push_function_token(TokenArray *arr, const char *name) {
    Token t;
    t.type = TOKEN_FUNCTION;
    strncpy(t.str, name, MAX_TOKEN_LEN-1);
    t.str[MAX_TOKEN_LEN-1] = '\0';
    token_array_push(arr, t);
}

void push_constant_token(TokenArray *arr, const char *name) {
    Token t;
    t.type = TOKEN_CONSTANT;
    strncpy(t.str, name, MAX_TOKEN_LEN-1);
    t.str[MAX_TOKEN_LEN-1] = '\0';
    // value will be set in conversion stage
    token_array_push(arr, t);
}

int tokenize_expression(const char *expr, TokenArray *out) {
    size_t len = strlen(expr);
    size_t i = 0;
    while (i < len) {
        char c = expr[i];
        if (isspace((unsigned char)c)) { i++; continue; }
        if (isdigit((unsigned char)c) || (c == '.' && i+1 < len && isdigit((unsigned char)expr[i+1]))) {
            // number literal (supports decimal)
            size_t j = i;
            int seen_dot = 0;
            if (expr[j] == '.') seen_dot = 1;
            while (j < len && (isdigit((unsigned char)expr[j]) || (!seen_dot && expr[j] == '.'))) {
                if (expr[j] == '.') seen_dot = 1;
                j++;
            }
            push_number_token(out, expr + i, j - i);
            i = j;
            continue;
        }
        if (is_operator_char(c)) {
            push_operator_token(out, c);
            i++;
            continue;
        }
        if (c == '(') {
            Token t; t.type = TOKEN_PAREN_LEFT; strncpy(t.str, "(", MAX_TOKEN_LEN-1); t.str[MAX_TOKEN_LEN-1] = '\0'; token_array_push(out, t); i++; continue;
        }
        if (c == ')') {
            Token t; t.type = TOKEN_PAREN_RIGHT; strncpy(t.str, ")", MAX_TOKEN_LEN-1); t.str[MAX_TOKEN_LEN-1] = '\0'; token_array_push(out, t); i++; continue;
        }
        if (c == ',') {
            Token t; t.type = TOKEN_COMMA; strncpy(t.str, ",", MAX_TOKEN_LEN-1); t.str[MAX_TOKEN_LEN-1] = '\0'; token_array_push(out, t); i++; continue;
        }
        if (is_identifier_char(c)) {
            size_t j = i;
            while (j < len && (is_identifier_char(expr[j]) || isdigit((unsigned char)expr[j]) || expr[j]=='.')) j++;
            size_t namelen = j - i;
            char name[MAX_TOKEN_LEN];
            if (namelen >= MAX_TOKEN_LEN) namelen = MAX_TOKEN_LEN-1;
            strncpy(name, expr + i, namelen);
            name[namelen] = '\0';
            if (is_function_name(name)) {
                push_function_token(out, name);
            } else if (is_constant_name(name)) {
                push_constant_token(out, name);
            } else {
                // treat unknown identifiers as function if followed by '(' else error
                // But allow custom function names like nCr, nPr (they're in is_function_name)
                push_function_token(out, name); // hoping it's a function or will error later
            }
            i = j;
            continue;
        }
        // Unknown character
        fprintf(stderr, "Tokenizer error: unexpected character '%c'\n", c);
        return 0;
    }
    return 1;
}

/* ---------- Shunting-yard (infix -> RPN) ---------- */

typedef struct {
    Token *data;
    int size;
    int capacity;
} TokenStack;

void tokenstack_init(TokenStack *s) {
    s->capacity = 256;
    s->size = 0;
    s->data = (Token*)malloc(sizeof(Token) * s->capacity);
    if (!s->data) { perror("malloc"); exit(1); }
}
void tokenstack_push(TokenStack *s, Token t) {
    if (s->size >= s->capacity) {
        s->capacity *= 2;
        s->data = (Token*)realloc(s->data, sizeof(Token) * s->capacity);
        if (!s->data) { perror("realloc"); exit(1); }
    }
    s->data[s->size++] = t;
}
Token tokenstack_pop(TokenStack *s) {
    if (s->size == 0) { Token t; t.type = TOKEN_NUMBER; strcpy(t.str, "0"); t.value = 0; return t; }
    return s->data[--s->size];
}
Token tokenstack_peek(TokenStack *s) {
    if (s->size == 0) { Token t; t.type = TOKEN_NUMBER; strcpy(t.str, "0"); t.value = 0; return t; }
    return s->data[s->size-1];
}
int tokenstack_empty(TokenStack *s) { return s->size == 0; }
void tokenstack_free(TokenStack *s) { free(s->data); s->data = NULL; s->size = s->capacity = 0; }

int is_unary_operator(const TokenArray *tokens, int idx) {
    // unary + or - when at start or after left paren, operator, or comma
    if (tokens->data[idx].type != TOKEN_OPERATOR) return 0;
    char op = tokens->data[idx].str[0];
    if (!(op == '+' || op == '-')) return 0;
    if (idx == 0) return 1;
    TokenType prev = tokens->data[idx-1].type;
    if (prev == TOKEN_OPERATOR || prev == TOKEN_PAREN_LEFT || prev == TOKEN_COMMA || prev == TOKEN_FUNCTION) return 1;
    return 0;
}

int to_rpn(const TokenArray *in, TokenArray *out) {
    TokenStack opstack;
    tokenstack_init(&opstack);

    for (int i = 0; i < in->size; ++i) {
        Token t = in->data[i];
        if (t.type == TOKEN_NUMBER || t.type == TOKEN_CONSTANT) {
            token_array_push(out, t);
        } else if (t.type == TOKEN_FUNCTION) {
            tokenstack_push(&opstack, t);
        } else if (t.type == TOKEN_COMMA) {
            // pop until left paren encountered
            int found = 0;
            while (!tokenstack_empty(&opstack)) {
                Token top = tokenstack_peek(&opstack);
                if (top.type == TOKEN_PAREN_LEFT) { found = 1; break; }
                token_array_push(out, tokenstack_pop(&opstack));
            }
            if (!found) { fprintf(stderr, "Error: misplaced comma or mismatched parentheses\n"); tokenstack_free(&opstack); return 0; }
        } else if (t.type == TOKEN_OPERATOR) {
            // handle unary + and -
            int unary = is_unary_operator(in, i);
            if (unary) {
                // encode unary + as function "u+" and unary - as "u-"
                Token uTok;
                uTok.type = TOKEN_FUNCTION;
                if (t.str[0] == '+') strncpy(uTok.str, "uplus", MAX_TOKEN_LEN-1);
                else strncpy(uTok.str, "uminus", MAX_TOKEN_LEN-1);
                tokenstack_push(&opstack, uTok);
                continue;
            }
            char op = t.str[0];
            while (!tokenstack_empty(&opstack)) {
                Token top = tokenstack_peek(&opstack);
                if (top.type == TOKEN_OPERATOR) {
                    char topop = top.str[0];
                    int p1 = op_precedence(topop);
                    int p2 = op_precedence(op);
                    if ((op_right_associative(op) && p2 < p1) || (!op_right_associative(op) && p2 <= p1)) {
                        token_array_push(out, tokenstack_pop(&opstack));
                        continue;
                    }
                } else if (top.type == TOKEN_FUNCTION) {
                    // functions have higher precedence -> pop them
                    token_array_push(out, tokenstack_pop(&opstack));
                    continue;
                }
                break;
            }
            tokenstack_push(&opstack, t);
        } else if (t.type == TOKEN_PAREN_LEFT) {
            tokenstack_push(&opstack, t);
        } else if (t.type == TOKEN_PAREN_RIGHT) {
            int found_left = 0;
            while (!tokenstack_empty(&opstack)) {
                Token top = tokenstack_pop(&opstack);
                if (top.type == TOKEN_PAREN_LEFT) { found_left = 1; break; }
                token_array_push(out, top);
            }
            if (!found_left) { fprintf(stderr, "Error: mismatched parentheses\n"); tokenstack_free(&opstack); return 0; }
            // after popping left paren, if top of stack is function, pop it into output
            if (!tokenstack_empty(&opstack)) {
                Token top = tokenstack_peek(&opstack);
                if (top.type == TOKEN_FUNCTION) token_array_push(out, tokenstack_pop(&opstack));
            }
        } else {
            fprintf(stderr, "Unknown token in parsing: %s\n", t.str);
            tokenstack_free(&opstack);
            return 0;
        }
    }

    while (!tokenstack_empty(&opstack)) {
        Token top = tokenstack_pop(&opstack);
        if (top.type == TOKEN_PAREN_LEFT || top.type == TOKEN_PAREN_RIGHT) {
            fprintf(stderr, "Error: mismatched parentheses\n");
            tokenstack_free(&opstack);
            return 0;
        }
        token_array_push(out, top);
    }

    tokenstack_free(&opstack);
    return 1;
}

/* ---------- Evaluation of RPN ---------- */

typedef struct {
    double *data;
    int size;
    int capacity;
} DoubleStack;

void dstack_init(DoubleStack *s) {
    s->capacity = STACK_INIT_CAP;
    s->size = 0;
    s->data = (double*)malloc(sizeof(double) * s->capacity);
    if (!s->data) { perror("malloc"); exit(1); }
}
void dstack_push(DoubleStack *s, double v) {
    if (s->size >= s->capacity) {
        s->capacity *= 2;
        s->data = (double*)realloc(s->data, sizeof(double) * s->capacity);
        if (!s->data) { perror("realloc"); exit(1); }
    }
    s->data[s->size++] = v;
}
double dstack_pop(DoubleStack *s) {
    if (s->size == 0) { return 0.0; }
    return s->data[--s->size];
}
double dstack_peek(const DoubleStack *s) {
    if (s->size == 0) return 0.0;
    return s->data[s->size-1];
}
void dstack_free(DoubleStack *s) { free(s->data); s->data = NULL; s->size = s->capacity = 0; }

long long ll_gcd(long long a, long long b) {
    if (a < 0) a = -a;
    if (b < 0) b = -b;
    while (b) {
        long long t = a % b;
        a = b; b = t;
    }
    return a;
}
long long ll_lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return llabs(a / ll_gcd(a,b) * b);
}

double factorial_double(double x, int *err) {
    // We'll support factorial only for non-negative integers in this implementation.
    // If x is integer and >=0, compute; else set err.
    *err = 0;
    if (x < 0) { *err = 1; return 0.0; }
    double xi = floor(x + 0.5);
    if (fabs(x - xi) > 1e-9) { *err = 1; return 0.0; } // not an integer
    if (xi > 170) { // factorial grows huge; beyond double
        *err = 1;
        return 0.0;
    }
    long long n = (long long)xi;
    double res = 1.0;
    for (long long i = 2; i <= n; ++i) res *= (double)i;
    return res;
}

int eval_function_by_name(const char *name, DoubleStack *stack, double *out_val) {
    // returns 1 on success, 0 on error
    // functions use stack arguments: pop as needed (rightmost last).
    if (str_eq_nocase(name, "uplus")) {
        // unary plus: do nothing
        double a = dstack_pop(stack);
        dstack_push(stack, +a);
        return 1;
    } else if (str_eq_nocase(name, "uminus")) {
        double a = dstack_pop(stack);
        dstack_push(stack, -a);
        return 1;
    } else if (str_eq_nocase(name, "sin")) {
        double a = dstack_pop(stack);
        if (angle_mode == MODE_DEG) a = a * M_PI / 180.0;
        dstack_push(stack, sin(a)); return 1;
    } else if (str_eq_nocase(name, "cos")) {
        double a = dstack_pop(stack);
        if (angle_mode == MODE_DEG) a = a * M_PI / 180.0;
        dstack_push(stack, cos(a)); return 1;
    } else if (str_eq_nocase(name, "tan")) {
        double a = dstack_pop(stack);
        if (angle_mode == MODE_DEG) a = a * M_PI / 180.0;
        dstack_push(stack, tan(a)); return 1;
    } else if (str_eq_nocase(name, "asin")) {
        double a = dstack_pop(stack);
        double r = asin(a);
        if (angle_mode == MODE_DEG) r = r * 180.0 / M_PI;
        dstack_push(stack, r); return 1;
    } else if (str_eq_nocase(name, "acos")) {
        double a = dstack_pop(stack);
        double r = acos(a);
        if (angle_mode == MODE_DEG) r = r * 180.0 / M_PI;
        dstack_push(stack, r); return 1;
    } else if (str_eq_nocase(name, "atan")) {
        double a = dstack_pop(stack);
        double r = atan(a);
        if (angle_mode == MODE_DEG) r = r * 180.0 / M_PI;
        dstack_push(stack, r); return 1;
    } else if (str_eq_nocase(name, "sinh")) {
        double a = dstack_pop(stack); dstack_push(stack, sinh(a)); return 1;
    } else if (str_eq_nocase(name, "cosh")) {
        double a = dstack_pop(stack); dstack_push(stack, cosh(a)); return 1;
    } else if (str_eq_nocase(name, "tanh")) {
        double a = dstack_pop(stack); dstack_push(stack, tanh(a)); return 1;
    } else if (str_eq_nocase(name, "sqrt")) {
        double a = dstack_pop(stack);
        if (a < 0) return 0;
        dstack_push(stack, sqrt(a)); return 1;
    } else if (str_eq_nocase(name, "cbrt")) {
        double a = dstack_pop(stack); dstack_push(stack, cbrt(a)); return 1;
    } else if (str_eq_nocase(name, "ln")) {
        double a = dstack_pop(stack);
        if (a <= 0) return 0;
        dstack_push(stack, log(a)); return 1;
    } else if (str_eq_nocase(name, "log")) {
        double a = dstack_pop(stack);
        if (a <= 0) return 0;
        dstack_push(stack, log10(a)); return 1;
    } else if (str_eq_nocase(name, "exp")) {
        double a = dstack_pop(stack); dstack_push(stack, exp(a)); return 1;
    } else if (str_eq_nocase(name, "pow")) {
        double b = dstack_pop(stack); double a = dstack_pop(stack);
        dstack_push(stack, pow(a, b)); return 1;
    } else if (str_eq_nocase(name, "abs")) {
        double a = dstack_pop(stack); dstack_push(stack, fabs(a)); return 1;
    } else if (str_eq_nocase(name, "floor")) {
        double a = dstack_pop(stack); dstack_push(stack, floor(a)); return 1;
    } else if (str_eq_nocase(name, "ceil")) {
        double a = dstack_pop(stack); dstack_push(stack, ceil(a)); return 1;
    } else if (str_eq_nocase(name, "fact") || str_eq_nocase(name, "factorial")) {
        double a = dstack_pop(stack);
        int err = 0;
        double f = factorial_double(a, &err);
        if (err) return 0;
        dstack_push(stack, f); return 1;
    } else if (str_eq_nocase(name, "nCr")) {
        double k = dstack_pop(stack);
        double n = dstack_pop(stack);
        long long ni = (long long)floor(n + 0.5);
        long long ki = (long long)floor(k + 0.5);
        if (ni < 0 || ki < 0 || ki > ni) return 0;
        // compute nCk safely
        double res = 1.0;
        if (ki > ni - ki) ki = ni - ki;
        for (long long i = 1; i <= ki; ++i) {
            res = res * (ni - ki + i) / (double)i;
        }
        dstack_push(stack, res); return 1;
    } else if (str_eq_nocase(name, "nPr")) {
        double k = dstack_pop(stack);
        double n = dstack_pop(stack);
        long long ni = (long long)floor(n + 0.5);
        long long ki = (long long)floor(k + 0.5);
        if (ni < 0 || ki < 0 || ki > ni) return 0;
        double res = 1.0;
        for (long long i = 0; i < ki; ++i) res *= (double)(ni - i);
        dstack_push(stack, res); return 1;
    } else if (str_eq_nocase(name, "gcd")) {
        double b = dstack_pop(stack); double a = dstack_pop(stack);
        long long ai = (long long)llround(a);
        long long bi = (long long)llround(b);
        long long g = ll_gcd(ai, bi);
        dstack_push(stack, (double)g); return 1;
    } else if (str_eq_nocase(name, "lcm")) {
        double b = dstack_pop(stack); double a = dstack_pop(stack);
        long long ai = (long long)llround(a);
        long long bi = (long long)llround(b);
        long long l = ll_lcm(ai, bi);
        dstack_push(stack, (double)l); return 1;
    } else {
        // Unknown function - try to treat as variable or error
        return 0;
    }
    return 0;
}

int evaluate_rpn(const TokenArray *rpn, double *result) {
    DoubleStack st;
    dstack_init(&st);
    for (int i = 0; i < rpn->size; ++i) {
        Token t = rpn->data[i];
        if (t.type == TOKEN_NUMBER) {
            dstack_push(&st, t.value);
        } else if (t.type == TOKEN_CONSTANT) {
            if (str_eq_nocase(t.str, "pi")) dstack_push(&st, M_PI);
            else if (str_eq_nocase(t.str, "e")) dstack_push(&st, M_E);
            else if (str_eq_nocase(t.str, "M")) dstack_push(&st, memory_slot);
            else {
                fprintf(stderr, "Unknown constant: %s\n", t.str);
                dstack_free(&st); return 0;
            }
        } else if (t.type == TOKEN_OPERATOR) {
            char op = t.str[0];
            double b = dstack_pop(&st);
            double a = dstack_pop(&st);
            double res = 0.0;
            switch (op) {
                case '+': res = a + b; break;
                case '-': res = a - b; break;
                case '*': res = a * b; break;
                case '/':
                    if (b == 0.0) { fprintf(stderr, "Math error: division by zero\n"); dstack_free(&st); return 0; }
                    res = a / b; break;
                case '%':
                    if (b == 0.0) { fprintf(stderr, "Math error: modulo by zero\n"); dstack_free(&st); return 0; }
                    res = fmod(a, b); break;
                case '^':
                    res = pow(a, b); break;
                default:
                    fprintf(stderr, "Unknown operator: %c\n", op);
                    dstack_free(&st); return 0;
            }
            dstack_push(&st, res);
        } else if (t.type == TOKEN_FUNCTION) {
            if (!eval_function_by_name(t.str, &st, NULL)) {
                fprintf(stderr, "Error evaluating function: %s\n", t.str);
                dstack_free(&st); return 0;
            }
        } else {
            fprintf(stderr, "Unexpected token in RPN evaluation: %s\n", t.str);
            dstack_free(&st); return 0;
        }
    }

    if (st.size != 1) {
        fprintf(stderr, "Evaluation error: stack has %d elements after evaluation\n", st.size);
        dstack_free(&st);
        return 0;
    }

    *result = dstack_pop(&st);
    dstack_free(&st);
    return 1;
}

/* ---------- Command history ---------- */

typedef struct {
    char **entries;
    int size;
    int capacity;
} History;

void history_init(History *h) {
    h->capacity = HISTORY_SIZE;
    h->size = 0;
    h->entries = (char**)malloc(sizeof(char*) * h->capacity);
    for (int i = 0; i < h->capacity; i++) h->entries[i] = NULL;
}
void history_add(History *h, const char *entry) {
    free(h->entries[h->size % h->capacity]);
    h->entries[h->size % h->capacity] = strdup(entry);
    h->size++;
}
void history_print(const History *h) {
    for (int i = 0; i < h->size && i < h->capacity; i++) {
        int idx = (h->size + i) % h->capacity;
        printf("%d: %s\n", i+1, h->entries[idx]);
    }
}
void history_free(History *h) {
    for (int i = 0; i < h->capacity; i++) {
        free(h->entries[i]);
        h->entries[i] = NULL;
    }
    free(h->entries);
    h->entries = NULL;
    h->size = h->capacity = 0;
}

/* ---------- Main calculator logic ---------- */

void print_help() {
    printf("Big Calculator - Help:\n");
    printf("Basic usage: <number> <operator> <number>  (e.g. 3 + 4)\n");
    printf("Operators: + - * / ^ %%\n");
    printf("Functions: sin cos tan asin acos atan sinh cosh tanh sqrt cbrt ln log exp pow abs floor ceil fact nCr nPr gcd lcm\n");
    printf("Constants: pi e M (memory recall)\n");
    printf("Angle mode: mode rad|deg (default is rad)\n");
    printf("Memory: m+ <value>, m- <value>, mr (recall), mc (clear)\n");
    printf("History: h (show), h <n> (show last n), !<n> (recall n), !! (repeat last)\n");
    printf("Help: ? or help\n");
}

int main(int argc, char **argv) {
    printf("Big Calculator - Type ? or help for help\n");

    History history;
    history_init(&history);

    char line[8192];
    while (1) {
        printf("> ");
        if (!fgets(line, sizeof(line), stdin)) break;
        trim_trailing_newline(line);

        if (str_eq_nocase(line, "exit") || str_eq_nocase(line, "quit")) break;

        if (line[0] == '?') {
            print_help();
            continue;
        }

        if (str_eq_nocase(line, "mode rad")) {
            angle_mode = MODE_RAD;
            printf("Angle mode set to RADIANS\n");
            continue;
        }
        if (str_eq_nocase(line, "mode deg")) {
            angle_mode = MODE_DEG;
            printf("Angle mode set to DEGREES\n");
            continue;
        }

        if (strlen(line) > 1 && line[0] == 'm' && (line[1] == '+' || line[1] == '-')) {
            // Memory operations
            char op = line[1];
            char *endptr;
            double value = strtod(line+2, &endptr);
            if (endptr == line+2 || *endptr != '\0') {
                fprintf(stderr, "Invalid memory operation\n");
                continue;
            }
            if (op == '+') memory_slot += value;
            else memory_slot -= value;
            printf("Memory slot %s: %.10g\n", (op == '+') ? "added to" : "subtracted from", fabs(value));
            continue;
        }

        if (str_eq_nocase(line, "mr")) {
            printf("Memory recall: %.10g\n", memory_slot);
            continue;
        }
        if (str_eq_nocase(line, "mc")) {
            memory_slot = 0.0;
            printf("Memory cleared\n");
            continue;
        }

        if (str_eq_nocase(line, "h")) {
            history_print(&history);
            continue;
        }

        // Tokenize the input expression
        TokenArray tokens;
        token_array_init(&tokens);
        if (!tokenize_expression(line, &tokens)) {
            fprintf(stderr, "Invalid expression: %s\n", line);
            token_array_free(&tokens);
            continue;
        }

        // Add to history
        history_add(&history, line);

        // Convert to RPN using shunting-yard
        TokenArray rpn;
        token_array_init(&rpn);
        if (!to_rpn(&tokens, &rpn)) {
            fprintf(stderr, "Error converting to RPN\n");
            token_array_free(&tokens);
            token_array_free(&rpn);
            continue;
        }

        // Evaluate the RPN expression
        double result = 0.0;
        if (!evaluate_rpn(&rpn, &result)) {
            fprintf(stderr, "Error evaluating expression\n");
            token_array_free(&tokens);
            token_array_free(&rpn);
            continue;
        }

        printf("Result: %.10g\n", result);

        token_array_free(&tokens);
        token_array_free(&rpn);
    }

    history_free(&history);

    printf("Goodbye!\n");
    return 0;
}