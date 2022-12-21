
struct stack_node {
    void* value;
    struct stack_node* next;
};

extern struct stack_node* __michelson_stack;

// TODO: Change this to be after static data and uninitialized data
void* __heap_start = 0;

void* malloc(unsigned long size) {
    // TODO: Move this to a proper malloc implementation and build a gc
    void* ptr = __heap_start;
    __heap_start += size;
    return ptr;
}

extern void log(void *);

void inspect_stack() {
    struct stack_node* node = __michelson_stack;
    while (node) {
        log(node->value);
        node = node->next;
    }
}

void michelson_push(void* value) {
    struct stack_node* node = malloc(sizeof(struct stack_node));
    node->value = value;
    node->next = __michelson_stack;
    __michelson_stack = node;
}

void michelson_dup_n(unsigned long n) {
    struct stack_node* node = __michelson_stack;

    while (n) {
        n--;
        node = node->next;
    }

    michelson_push(node->value);
}

void michelson_drop_n(unsigned long n) {
    struct stack_node* node = __michelson_stack;

    while (n) {
        n--;
        node = node->next;
    }

    __michelson_stack = node;
}

void michelson_dug_n(unsigned long n) {
    struct stack_node* node;
    struct stack_node* head = node = __michelson_stack;

    while (n) {
        n--;
        node = node->next;
    }

    // TODO: This shouldn't use mutability
    __michelson_stack = head->next;
    head->next = node->next;
    node->next = head;
}

void michelson_dig_n(unsigned long n) {
    struct stack_node* node = __michelson_stack;
    struct stack_node* a;

    while (n) {
        n--;
        node = node->next;
    }

    // TODO: This shouldn't use mutability
    a = node->next;
    node->next = a->next;
    a->next = __michelson_stack;
    __michelson_stack = a;
}

extern void main();

void _start() {
    main();
}