struct stack_node {
    void* value;
    struct stack_node* next;
};

extern struct stack_node* stack;

// TODO: Change this to be after static data and uninitialized data
void* __heap_start = 0;

void* malloc(unsigned long size) {
    // TODO: Move this to a proper malloc implementation and build a gc
    void* ptr = __heap_start;
    __heap_start += size;
    return ptr;
}

extern void main();

void _start() {
    main();
}