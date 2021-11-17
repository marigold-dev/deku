var storage = 0;

function add(a) {
    storage += a
}

function sub(a) {
    storage -= a
}

// We're calling this function reset instead of default
// because `default` is a javascript keyword
function reset() {
    storage = 0;
}