#include </nix/store/7vvbds8bhify8ri7xnwl65w60v9ij929-coz-0.2.1/include/coz.h>

void ccozBegin(char* name) {
    COZ_BEGIN(name);
}

void ccozEnd(char* name) {
    COZ_END(name);
}

void ccozProgressNamed(char* name) {
    COZ_PROGRESS_NAMED(name);
}

void ccozProgress() {
    COZ_PROGRESS;
}

