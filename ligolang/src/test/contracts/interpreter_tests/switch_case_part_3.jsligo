// 2 case 1 default - 3 * 3 * 2 -> 18 cases

let case1 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 3:
        case 2:
            output = output + " World";
        default:
    };
    output = output + " end";
    return output;
};
let _test1 = () : unit => {
    let _1 = case1(1);
    let _2 = case1(2);
    let _3 = case1(3);
    let _4 = case1(4);
    assert (_1 == "Hello World end");
    assert (_2 == " World end");
    assert (_3 == " World end");
    assert (_4 == " end");
};
let test1 = _test1(); 

let case2 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
        case 3:
        case 2:
            output = output + " World";
            break;
        default:
    };
    output = output + " end";
    return output;
};
let _test2 = () : unit => {
    let _1 = case2(1);
    let _2 = case2(2);
    let _3 = case2(3);
    let _4 = case2(4);
    assert (_1 == "Hello World end");
    assert (_2 == " World end");
    assert (_3 == " World end");
    assert (_4 == " end");
};
let test2 = _test2();

let case3 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
        case 3:
        case 2:
            output = output + " World";
            return output;
        default:
    };
    output = output + " end";
    return output;
};
let _test3 = () : unit => {
    let _1 = case3(1);
    let _2 = case3(2);
    let _3 = case3(3);
    let _4 = case3(4);
    assert (_1 == "Hello World");
    assert (_2 == " World");
    assert (_3 == " World");
    assert (_4 == " end");
};
let test3 = _test3();

let case4 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
            break;
        case 3:
        case 2:
            output = output + " World";
        default:
    };
    output = output + " end";
    return output;
};
let _test4 = () : unit => {
    let _1 = case4(1);
    let _2 = case4(2);
    let _3 = case4(3);
    let _4 = case4(4);
    assert (_1 == "Hello end");
    assert (_2 == " World end");
    assert (_3 == " World end");
    assert (_4 == " end");
};
let test4 = _test4();

let case5 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
            break;
        case 3:
        case 2:
            output = output + " World";
            break;
        default:
    };
    output = output + " end";
    return output;
};
let _test5 = () : unit => {
    let _1 = case5(1);
    let _2 = case5(2);
    let _3 = case5(3);
    let _4 = case5(4);
    assert (_1 == "Hello end");
    assert (_2 == " World end");
    assert (_3 == " World end");
    assert (_4 == " end");
};
let test5 = _test5();

let case6 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
            break;
        case 3:
        case 2:
            output = output + " World";
            return output;
        default:
    };
    output = output + " end";
    return output;
};
let _test6 = () : unit => {
    let _1 = case6(1);
    let _2 = case6(2);
    let _3 = case6(3);
    let _4 = case6(4);
    assert (_1 == "Hello end");
    assert (_2 == " World");
    assert (_3 == " World");
    assert (_4 == " end");
};
let test6 = _test6();

let case7 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
            return output;
        case 3:
        case 2:
            output = output + " World";
        default:
    };
    output = output + " end";
    return output;
};
let _test7 = () : unit => {
    let _1 = case7(1);
    let _2 = case7(2);
    let _3 = case7(3);
    let _4 = case7(4);
    assert (_1 == "Hello");
    assert (_2 == " World end");
    assert (_3 == " World end");
    assert (_4 == " end");
};
let test7 = _test7();

let case8 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
            return output;
        case 3:
        case 2:
            output = output + " World";
            break;
        default:
    };
    output = output + " end";
    return output;
};
let _test8 = () : unit => {
    let _1 = case8(1);
    let _2 = case8(2);
    let _3 = case8(3);
    let _4 = case8(4);
    assert (_1 == "Hello");
    assert (_2 == " World end");
    assert (_3 == " World end");
    assert (_4 == " end");
};
let test8 = _test8();

let case9 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
            return output;
        case 3:
        case 2:
            output = output + " World";
            return output;
        default:
    };
    output = output + " end";
    return output;
};
let _test9 = () : unit => {
    let _1 = case9(1);
    let _2 = case9(2);
    let _3 = case9(3);
    let _4 = case9(4);
    assert (_1 == "Hello");
    assert (_2 == " World");
    assert (_3 == " World");
    assert (_4 == " end");
};
let test9 = _test9();

let case10 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
        case 3:
        case 2:
            output = output + " World";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test10 = () : unit => {
    let _1 = case10(1);
    let _2 = case10(2);
    let _3 = case10(3);
    let _4 = case10(4);
    assert (_1 == "Hello World !!!");
    assert (_2 == " World !!!");
    assert (_3 == " World !!!");
    assert (_4 == " !!!");
};
let test10 = _test10();

let case11 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
        case 3:
        case 2:
            output = output + " World";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test11 = () : unit => {
    let _1 = case11(1);
    let _2 = case11(2);
    let _3 = case11(3);
    let _4 = case11(4);
    assert (_1 == "Hello World end");
    assert (_2 == " World end");
    assert (_3 == " World end");
    assert (_4 == " !!!");
};
let test11 = _test11();

let case12 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello"; 
        case 3:
        case 2:
            output = output + " World";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test12 = () : unit => {
    let _1 = case12(1);
    let _2 = case12(2);
    let _3 = case12(3);
    let _4 = case12(4);
    assert (_1 == "Hello World");
    assert (_2 == " World");
    assert (_3 == " World");
    assert (_4 == " !!!");
};
let test12 = _test12();

let case13 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 3:
        case 2:
            output = output + " World";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test13 = () : unit => {
    let _1 = case13(1);
    let _2 = case13(2);
    let _3 = case13(3);
    let _4 = case13(4);
    assert (_1 == "Hello end");
    assert (_2 == " World !!!");
    assert (_3 == " World !!!");
    assert (_4 == " !!!");
};
let test13 = _test13();

let case14 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 3:
        case 2:
            output = output + " World";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test14 = () : unit => {
    let _1 = case14(1);
    let _2 = case14(2);
    let _3 = case14(3);
    let _4 = case14(4);
    assert (_1 == "Hello end");
    assert (_2 == " World end");
    assert (_3 == " World end");
    assert (_4 == " !!!");
};
let test14 = _test14();

let case15 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 3:
        case 2:
            output = output + " World";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test15 = () : unit => {
    let _1 = case15(1);
    let _2 = case15(2);
    let _3 = case15(3);
    let _4 = case15(4);
    assert (_1 == "Hello end");
    assert (_2 == " World");
    assert (_3 == " World");
    assert (_4 == " !!!");
};
let test15 = _test15();

let case16 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 3:
        case 2:
            output = output + " World";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test16 = () : unit => {
    let _1 = case16(1);
    let _2 = case16(2);
    let _3 = case16(3);
    let _4 = case16(4);
    assert (_1 == "Hello");
    assert (_2 == " World !!!");
    assert (_3 == " World !!!");
    assert (_4 == " !!!");
};
let test16 = _test16();

let case17 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 3:
        case 2:
            output = output + " World";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test17 = () : unit => {
    let _1 = case17(1);
    let _2 = case17(2);
    let _3 = case17(3);
    let _4 = case17(4);
    assert (_1 == "Hello");
    assert (_2 == " World end");
    assert (_3 == " World end");
    assert (_4 == " !!!");
};
let test17 = _test17();

let case18 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 3:
        case 2:
            output = output + " World";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test18 = () : unit => {
    let _1 = case18(1);
    let _2 = case18(2);
    let _3 = case18(3);
    let _4 = case18(4);
    assert (_1 == "Hello");
    assert (_2 == " World");
    assert (_3 == " World");
    assert (_4 == " !!!");
};
let test18 = _test18();

let case19 = (n : int) : string => {
    let output = "";
    switch (n) {
        default:
    };
    output = output + " end";
    return output;
};
let _test19 = () : unit => {
    let _1 = case19(1);
    assert (_1 == " end");
};
let test19 = _test19();
