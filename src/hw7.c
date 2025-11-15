#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    bst_sf *node = malloc(sizeof(bst_sf));
    assert(node != NULL);
    node->mat = mat;
    node->left_child = NULL;
    node->right_child = NULL;

    if (root == NULL) {
        return node;
    }

    bst_sf *curr = root;
    while (1) {
        if (mat->name < curr->mat->name) {
            if (curr->left_child == NULL) {
                curr->left_child = node;
                break;
            } else {
                curr = curr->left_child;
            }
        } else {
            if (curr->right_child == NULL) {
                curr->right_child = node;
                break;
            } else {
                curr = curr->right_child;
            }
        }
    }

    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    bst_sf *curr = root;
    while (curr != NULL) {
        if (name == curr->mat->name) {
            return curr->mat;
        } else if (name < curr->mat->name) {
            curr = curr->left_child;
        } else {
            curr = curr->right_child;
        }
    }
    return NULL;
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) return;
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    assert(mat1 != NULL && mat2 != NULL);
    assert(mat1->num_rows == mat2->num_rows);
    assert(mat1->num_cols == mat2->num_cols);

    unsigned int rows = mat1->num_rows;
    unsigned int cols = mat1->num_cols;
    matrix_sf *res = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    assert(res != NULL);
    res->name = '?';
    res->num_rows = rows;
    res->num_cols = cols;

    unsigned int total = rows * cols;
    for (unsigned int i = 0; i < total; i++) {
        res->values[i] = mat1->values[i] + mat2->values[i];
    }

    return res;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    assert(mat1 != NULL && mat2 != NULL);
    assert(mat1->num_cols == mat2->num_rows);

    unsigned int rows = mat1->num_rows;
    unsigned int inner = mat1->num_cols;
    unsigned int cols = mat2->num_cols;

    matrix_sf *res = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    assert(res != NULL);
    res->name = '?';
    res->num_rows = rows;
    res->num_cols = cols;

    for (unsigned int r = 0; r < rows; r++) {
        for (unsigned int c = 0; c < cols; c++) {
            long sum = 0;
            for (unsigned int k = 0; k < inner; k++) {
                int a = mat1->values[r * inner + k];
                int b = mat2->values[k * cols + c];
                sum += (long)a * (long)b;
            }
            res->values[r * cols + c] = (int)sum;
        }
    }

    return res;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    assert(mat != NULL);
    unsigned int rows = mat->num_rows;
    unsigned int cols = mat->num_cols;

    matrix_sf *res = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    assert(res != NULL);
    res->name = '?';
    res->num_rows = cols;
    res->num_cols = rows;

    for (unsigned int r = 0; r < rows; r++) {
        for (unsigned int c = 0; c < cols; c++) {
            int v = mat->values[r * cols + c];
            res->values[c * rows + r] = v;
        }
    }

    return res;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    assert(expr != NULL);

    const char *p = expr;
    while (*p && isspace((unsigned char)*p)) p++;
    assert(isdigit((unsigned char)*p));
    char *endptr;
    unsigned long nr = strtoul(p, &endptr, 10);
    p = endptr;

    while (*p && isspace((unsigned char)*p)) p++;
    assert(isdigit((unsigned char)*p));
    unsigned long nc = strtoul(p, &endptr, 10);
    p = endptr;

    unsigned int num_rows = (unsigned int)nr;
    unsigned int num_cols = (unsigned int)nc;

    matrix_sf *m = malloc(sizeof(matrix_sf) + num_rows * num_cols * sizeof(int));
    assert(m != NULL);
    m->name = name;
    m->num_rows = num_rows;
    m->num_cols = num_cols;

    while (*p && *p != '[') p++;
    if (*p == '[') p++;

    unsigned int total = num_rows * num_cols;
    for (unsigned int i = 0; i < total; ) {
        while (*p && !(isdigit((unsigned char)*p) || *p == '-' || *p == '+')) {
            p++;
        }
        if (!*p) break; // safety

        long val = strtol(p, &endptr, 10);
        m->values[i++] = (int)val;
        p = endptr;
    }

    return m;
}

static int precedence(char op) {
    if (op == '\'') return 3;
    if (op == '*')  return 2;
    if (op == '+')  return 1;
    return 0;
}

char* infix2postfix_sf(char *infix) {
    assert(infix != NULL);
    size_t n = strlen(infix);

    char *out = malloc(n + 1);
    assert(out != NULL);
    char *opstack = malloc(n);
    assert(opstack != NULL);
    int top = -1;
    size_t out_len = 0;

    for (size_t i = 0; i < n; i++) {
        char c = infix[i];

        if (isspace((unsigned char)c)) {
            continue;
        } else if (isalpha((unsigned char)c)) {
            out[out_len++] = c;
        } else if (c == '(') {
            opstack[++top] = c;
        } else if (c == ')') {
            while (top >= 0 && opstack[top] != '(') {
                out[out_len++] = opstack[top--];
            }
            if (top >= 0 && opstack[top] == '(') {
                top--;
            }
        } else if (c == '+' || c == '*' || c == '\'') {
            int prec_c = precedence(c);
            while (top >= 0 && opstack[top] != '(' &&
                   precedence(opstack[top]) >= prec_c) {
                out[out_len++] = opstack[top--];
            }
            opstack[++top] = c;
        } else {
        }
    }

    while (top >= 0) {
        if (opstack[top] != '(' && opstack[top] != ')') {
            out[out_len++] = opstack[top];
        }
        top--;
    }

    out[out_len] = '\0';
    free(opstack);
    return out;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    assert(expr != NULL);

    char *postfix = infix2postfix_sf(expr);
    size_t len = strlen(postfix);

    matrix_sf **stack = malloc(len * sizeof(matrix_sf*));
    assert(stack != NULL);
    size_t top = 0;

    int has_operator = 0;

    for (size_t i = 0; i < len; i++) {
        char c = postfix[i];

        if (isalpha((unsigned char)c)) {
            matrix_sf *m = find_bst_sf(c, root);
            assert(m != NULL);
            stack[top++] = m;
        } else if (c == '\'') {
            assert(top >= 1);
            matrix_sf *a = stack[--top];
            matrix_sf *res = transpose_mat_sf(a);
            if (!isalpha((unsigned char)a->name)) {
                free(a);
            }
            stack[top++] = res;
            has_operator = 1;
        } else if (c == '+' || c == '*') {
            assert(top >= 2);
            matrix_sf *b = stack[--top];
            matrix_sf *a = stack[--top];

            matrix_sf *res = NULL;
            if (c == '+') {
                res = add_mats_sf(a, b);
            } else {
                res = mult_mats_sf(a, b);
            }

            if (!isalpha((unsigned char)a->name)) {
                free(a);
            }
            if (!isalpha((unsigned char)b->name)) {
                free(b);
            }

            stack[top++] = res;
            has_operator = 1;
        } else {
        }
    }

    assert(top == 1);
    matrix_sf *result = stack[0];

    if (!has_operator && isalpha((unsigned char)result->name)) {
        matrix_sf *copy = copy_matrix(result->num_rows, result->num_cols, result->values);
        copy->name = name;
        result = copy;
    } else {
        result->name = name;
    }

    free(stack);
    free(postfix);

    return result;
}

matrix_sf *execute_script_sf(char *filename) {
    assert(filename != NULL);
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        return NULL;
    }

    bst_sf *root = NULL;
    matrix_sf *last = NULL;

    char *line = NULL;
    size_t bufsize = MAX_LINE_LEN;
    ssize_t linelen;

    while ((linelen = getline(&line, &bufsize, file)) != -1) {
        char *p = line;
        while (*p && isspace((unsigned char)*p)) p++;
        if (*p == '\0') {
            continue;
        }

        char name = *p;
        p++;

        while (*p && isspace((unsigned char)*p)) p++;
        if (*p != '=') {
            continue;
        }
        p++;

        while (*p && isspace((unsigned char)*p)) p++;
        if (*p == '\0') {
            continue;
        }

        if (isdigit((unsigned char)*p)) {
            matrix_sf *m = create_matrix_sf(name, p);
            root = insert_bst_sf(m, root);
            last = m;
        } else {
            matrix_sf *m = evaluate_expr_sf(name, p, root);
            root = insert_bst_sf(m, root);
            last = m;
        }
    }

    free(line);
    fclose(file);

    return last;
}

matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    assert(m != NULL);
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
