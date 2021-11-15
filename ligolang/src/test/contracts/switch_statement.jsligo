
let single_default_return = (n : int) : string => {
    let output = "Hello";
    switch (n) {
      default: 
         output = output + "!!";
         return output; 
    };
    output = output + "World"; 
    return output;
};

let single_default_no_statements = (n : int) : string => {
    let output = "Hello";
    switch (n) {
      default: 
    }
    return output;
};

let single_default_break_1 = (n : int) : string => {
    let output = "Hello";
    switch (n) {
      default: output = output + "World"; 
    }
    return output;
};

let single_default_break_2 = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      default: 
        output = output + "World"; 
        break;
    }
    return output;
};

let single_case_no_statements = (n : int) : string => {
    let output = "Hello";
    switch (n) {
      case 1: 
    }
    return output;
};

let single_case_return = (n : int) : string => {
    let output = "Hello";
    switch (n) {
      case 1: return "World";
    };
    return output;
}

let single_case_fallthrough = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
    }
    return output;
}

let single_case_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        break;
    }
    return output;
}

let case_default_fallthrough_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
      default:
        output = output + "!!!";
        break;
    }
    return output;
};

let case_default_break_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        break;
      default:
        output = output + "!!!";
        break;
    }
    return output;
};

let case_default_return_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        return output;
      default:
        output = output + "!!!";
    };
    output = output + " ???";
    return output;
};

let case_default_fallthrough_return = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
      default:
        output = output + "!!!";
        return output;
    };
    output = output + " ???";
    return output;
};

let case_default_break_return = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        break;
      default:
        output = output + "!!!";
        return output;
    };
    output = output + " ???";
    return output;
};

let case_default_return_return = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        return output;
      default:
        output = output + "!!!";
        return output;
    };
    output = output + " ???";
    return output;
};

let case_case_fallthrough = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
      case 2:
        output = output + "!!!";
    };
    output = output + " ???";
    return output;
};

let case_case_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        break;
      case 2:
        output = output + "!!!";
    };
    output = output + " ???";
    return output;
};

let case_case_return = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        return output;
      case 2:
        output = output + "!!!";
    };
    output = output + " ???";
    return output;
};

let case_case_fallthrough_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
      case 2:
        output = output + "!!!";
        break;
    };
    output = output + " ???";
    return output;
};

let case_case_break_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        break;
      case 2:
        output = output + "!!!";
        break;
    };
    output = output + " ???";
    return output;
};

let case_case_return_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        return output;
      case 2:
        output = output + "!!!";
        break;
    };
    output = output + " ???";
    return output;
};
 
let case_case_fallthrough_return = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
      case 2:
        output = output + "!!!";
        return output;
    };
    output = output + " ???";
    return output;
};

let case_case_break_return = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        break;
      case 2:
        output = output + "!!!";
        return output;
    };
    output = output + " ???";
    return output;
};

let case_case_return_return = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        return output;
      case 2:
        output = output + "!!!";
        return output;
    };
    output = output + " ???";
    return output;
};

let case_all_fallthrough = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
      case 2:
        output = output + "!!!";
      case 3:
        output = output + "@@@";
    };
    output = output + " ???";
    return output;
};

let case_all_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        break;
      case 2:
        output = output + "!!!";
        break;
      case 3:
        output = output + "@@@";
        break;
    };
    output = output + " ???";
    return output;
};

let case_all_return = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        return output;
      case 2:
        output = output + "!!!";
        return output;
      case 3:
        output = output + "@@@";
        return output;
    };
    output = output + " ???";
    return output;
};

let case_default_all_fallthrough = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
      case 2:
        output = output + "!!!";
      case 3:
        output = output + "@@@";
      default:
        output = output + "###";
    };
    output = output + " ???";
    return output;
};

let case_default_all_break = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        break;
      case 2:
        output = output + "!!!";
        break;
      case 3:
        output = output + "@@@";
        break;
      default:
        output = output + "###";
    };
    output = output + " ???";
    return output;
};

let case_default_all_return = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        return output;
      case 2:
        output = output + "!!!";
        return output;
      case 3:
        output = output + "@@@";
        return output;
      default:
        output = output + "###";
        return output;
    };
    output = output + " ???";
    return output;
};

let case_default_all_fallthrough_4 = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
      case 2:
        output = output + "!!!";
      case 3:
        output = output + "@@@";
      case 4:
        output = output + "^^^";
      default:
        output = output + "###";
    };
    output = output + " ???";
    return output;
};

let case_default_all_break_4 = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        break;
      case 2:
        output = output + "!!!";
        break;
      case 3:
        output = output + "@@@";
        break;
      case 4:
        output = output + "^^^";
        break;
      default:
        output = output + "###";
    };
    output = output + " ???";
    return output;
};

let case_default_all_return_4 = (n : int) : string => {
    let output = "Hello ";
    switch (n) {
      case 1: 
        output = output + "World";
        return output;
      case 2:
        output = output + "!!!";
        return output;
      case 3:
        output = output + "@@@";
        return output;
      case 4:
        output = output + "^^^";
        return output;
      default:
        output = output + "###";
        return output;
    };
    output = output + " ???";
    return output;
};