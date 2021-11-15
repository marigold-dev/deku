// 3 case 1 default - 3 * 3 * 3 * 2 -> 54 cases

let case1 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test1 = () : unit => {
    let _1 = case1(1);
    let _2 = case1(2);
    let _3 = case1(3);
    let _4 = case1(4);
    assert (_1 == "Hello World @@@ !!! end");
    assert (_2 == " World @@@ !!! end");
    assert (_3 == " @@@ !!! end");
    assert (_4 == " !!! end");
};
let test1 = _test1();

let case2 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test2 = () : unit => {
    let _1 = case2(1);
    let _2 = case2(2);
    let _3 = case2(3);
    let _4 = case2(4);
    assert (_1 == "Hello World @@@ end");
    assert (_2 == " World @@@ end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!! end");
};
let test2 = _test2();

let case3 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test3 = () : unit => {
    let _1 = case3(1);
    let _2 = case3(2);
    let _3 = case3(3);
    let _4 = case3(4);
    assert (_1 == "Hello World @@@");
    assert (_2 == " World @@@");
    assert (_3 == " @@@");
    assert (_4 == " !!! end");
};
let test3 = _test3();

let case4 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test4 = () : unit => {
    let _1 = case4(1);
    let _2 = case4(2);
    let _3 = case4(3);
    let _4 = case4(4);
    assert (_1 == "Hello World end");
    assert (_2 == " World end");
    assert (_3 == " @@@ !!! end");
    assert (_4 == " !!! end");
};
let test4 = _test4();

let case5 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test5 = () : unit => {
    let _1 = case5(1);
    let _2 = case5(2);
    let _3 = case5(3);
    let _4 = case5(4);
    assert (_1 == "Hello World end");
    assert (_2 == " World end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!! end");
};
let test5 = _test5();

let case6 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test6 = () : unit => {
    let _1 = case6(1);
    let _2 = case6(2);
    let _3 = case6(3);
    let _4 = case6(4);
    assert (_1 == "Hello World end");
    assert (_2 == " World end");
    assert (_3 == " @@@");
    assert (_4 == " !!! end");
};
let test6 = _test6();

let case7 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test7 = () : unit => {
    let _1 = case7(1);
    let _2 = case7(2);
    let _3 = case7(3);
    let _4 = case7(4);
    assert (_1 == "Hello World");
    assert (_2 == " World");
    assert (_3 == " @@@ !!! end");
    assert (_4 == " !!! end");
};
let test7 = _test7();

let case8 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test8 = () : unit => {
    let _1 = case8(1);
    let _2 = case8(2);
    let _3 = case8(3);
    let _4 = case8(4);
    assert (_1 == "Hello World");
    assert (_2 == " World");
    assert (_3 == " @@@ end");
    assert (_4 == " !!! end");
};
let test8 = _test8();

let case9 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test9 = () : unit => {
    let _1 = case9(1);
    let _2 = case9(2);
    let _3 = case9(3);
    let _4 = case9(4);
    assert (_1 == "Hello World");
    assert (_2 == " World");
    assert (_3 == " @@@");
    assert (_4 == " !!! end");
};
let test9 = _test9();


let case10 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test10 = () : unit => {
    let _1 = case10(1);
    let _2 = case10(2);
    let _3 = case10(3);
    let _4 = case10(4);
    assert (_1 == "Hello end");
    assert (_2 == " World @@@ !!! end");
    assert (_3 == " @@@ !!! end");
    assert (_4 == " !!! end");
};
let test10 = _test10();

let case11 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test11 = () : unit => {
    let _1 = case11(1);
    let _2 = case11(2);
    let _3 = case11(3);
    let _4 = case11(4);
    assert (_1 == "Hello end");
    assert (_2 == " World @@@ end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!! end");
};
let test11 = _test11();

let case12 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test12 = () : unit => {
    let _1 = case12(1);
    let _2 = case12(2);
    let _3 = case12(3);
    let _4 = case12(4);
    assert (_1 == "Hello end");
    assert (_2 == " World @@@");
    assert (_3 == " @@@");
    assert (_4 == " !!! end");
};
let test12 = _test12();

let case13 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
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
    assert (_2 == " World end");
    assert (_3 == " @@@ !!! end");
    assert (_4 == " !!! end");
};
let test13 = _test13();

let case14 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
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
    assert (_3 == " @@@ end");
    assert (_4 == " !!! end");
};
let test14 = _test14();

let case15 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
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
    assert (_2 == " World end");
    assert (_3 == " @@@");
    assert (_4 == " !!! end");
};
let test15 = _test15();

let case16 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test16 = () : unit => {
    let _1 = case16(1);
    let _2 = case16(2);
    let _3 = case16(3);
    let _4 = case16(4);
    assert (_1 == "Hello end");
    assert (_2 == " World");
    assert (_3 == " @@@ !!! end");
    assert (_4 == " !!! end");
};
let test16 = _test16();

let case17 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test17 = () : unit => {
    let _1 = case17(1);
    let _2 = case17(2);
    let _3 = case17(3);
    let _4 = case17(4);
    assert (_1 == "Hello end");
    assert (_2 == " World");
    assert (_3 == " @@@ end");
    assert (_4 == " !!! end");
};
let test17 = _test17();

let case18 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test18 = () : unit => {
    let _1 = case18(1);
    let _2 = case18(2);
    let _3 = case18(3);
    let _4 = case18(4);
    assert (_1 == "Hello end");
    assert (_2 == " World");
    assert (_3 == " @@@");
    assert (_4 == " !!! end");
};
let test18 = _test18();

let case19 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test19 = () : unit => {
    let _1 = case19(1);
    let _2 = case19(2);
    let _3 = case19(3);
    let _4 = case19(4);
    assert (_1 == "Hello");
    assert (_2 == " World @@@ !!! end");
    assert (_3 == " @@@ !!! end");
    assert (_4 == " !!! end");
};
let test19 = _test19();

let case20 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test20 = () : unit => {
    let _1 = case20(1);
    let _2 = case20(2);
    let _3 = case20(3);
    let _4 = case20(4);
    assert (_1 == "Hello");
    assert (_2 == " World @@@ end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!! end");
};
let test20 = _test20();

let case21 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test21 = () : unit => {
    let _1 = case21(1);
    let _2 = case21(2);
    let _3 = case21(3);
    let _4 = case21(4);
    assert (_1 == "Hello");
    assert (_2 == " World @@@");
    assert (_3 == " @@@");
    assert (_4 == " !!! end");
};
let test21 = _test21();

let case22 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test22 = () : unit => {
    let _1 = case22(1);
    let _2 = case22(2);
    let _3 = case22(3);
    let _4 = case22(4);
    assert (_1 == "Hello");
    assert (_2 == " World end");
    assert (_3 == " @@@ !!! end");
    assert (_4 == " !!! end");
};
let test22 = _test22();

let case23 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test23 = () : unit => {
    let _1 = case23(1);
    let _2 = case23(2);
    let _3 = case23(3);
    let _4 = case23(4);
    assert (_1 == "Hello");
    assert (_2 == " World end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!! end");
};
let test23 = _test23();

let case24 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test24 = () : unit => {
    let _1 = case24(1);
    let _2 = case24(2);
    let _3 = case24(3);
    let _4 = case24(4);
    assert (_1 == "Hello");
    assert (_2 == " World end");
    assert (_3 == " @@@");
    assert (_4 == " !!! end");
};
let test24 = _test24();

let case25 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test25 = () : unit => {
    let _1 = case25(1);
    let _2 = case25(2);
    let _3 = case25(3);
    let _4 = case25(4);
    assert (_1 == "Hello");
    assert (_2 == " World");
    assert (_3 == " @@@ !!! end");
    assert (_4 == " !!! end");
};
let test25 = _test25();

let case26 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test26 = () : unit => {
    let _1 = case26(1);
    let _2 = case26(2);
    let _3 = case26(3);
    let _4 = case26(4);
    assert (_1 == "Hello");
    assert (_2 == " World");
    assert (_3 == " @@@ end");
    assert (_4 == " !!! end");
};
let test26 = _test26();


let case27 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
    };
    output = output + " end";
    return output;
};
let _test27 = () : unit => {
    let _1 = case27(1);
    let _2 = case27(2);
    let _3 = case27(3);
    let _4 = case27(4);
    assert (_1 == "Hello");
    assert (_2 == " World");
    assert (_3 == " @@@");
    assert (_4 == " !!! end");
};
let test27 = _test27();

let case28 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test28 = () : unit => {
    let _1 = case28(1);
    let _2 = case28(2);
    let _3 = case28(3);
    let _4 = case28(4);
    assert (_1 == "Hello World @@@ !!!");
    assert (_2 == " World @@@ !!!");
    assert (_3 == " @@@ !!!");
    assert (_4 == " !!!");
};
let test28 = _test28();

let case29 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test29 = () : unit => {
    let _1 = case29(1);
    let _2 = case29(2);
    let _3 = case29(3);
    let _4 = case29(4);
    assert (_1 == "Hello World @@@ end");
    assert (_2 == " World @@@ end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!!");
};
let test29 = _test29();

let case30 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test30 = () : unit => {
    let _1 = case30(1);
    let _2 = case30(2);
    let _3 = case30(3);
    let _4 = case30(4);
    assert (_1 == "Hello World @@@");
    assert (_2 == " World @@@");
    assert (_3 == " @@@");
    assert (_4 == " !!!");
};
let test30 = _test30();

let case31 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test31 = () : unit => {
    let _1 = case31(1);
    let _2 = case31(2);
    let _3 = case31(3);
    let _4 = case31(4);
    assert (_1 == "Hello World end");
    assert (_2 == " World end");
    assert (_3 == " @@@ !!!");
    assert (_4 == " !!!");
};
let test31 = _test31();

let case32 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test32 = () : unit => {
    let _1 = case32(1);
    let _2 = case32(2);
    let _3 = case32(3);
    let _4 = case32(4);
    assert (_1 == "Hello World end");
    assert (_2 == " World end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!!");
};
let test32 = _test32();

let case33 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test33 = () : unit => {
    let _1 = case33(1);
    let _2 = case33(2);
    let _3 = case33(3);
    let _4 = case33(4);
    assert (_1 == "Hello World end");
    assert (_2 == " World end");
    assert (_3 == " @@@");
    assert (_4 == " !!!");
};
let test33 = _test33();

let case34 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test34 = () : unit => {
    let _1 = case34(1);
    let _2 = case34(2);
    let _3 = case34(3);
    let _4 = case34(4);
    assert (_1 == "Hello World");
    assert (_2 == " World");
    assert (_3 == " @@@ !!!");
    assert (_4 == " !!!");
};
let test34 = _test34();

let case35 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test35 = () : unit => {
    let _1 = case35(1);
    let _2 = case35(2);
    let _3 = case35(3);
    let _4 = case35(4);
    assert (_1 == "Hello World");
    assert (_2 == " World");
    assert (_3 == " @@@ end");
    assert (_4 == " !!!");
};
let test35 = _test35();

let case36 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test36 = () : unit => {
    let _1 = case36(1);
    let _2 = case36(2);
    let _3 = case36(3);
    let _4 = case36(4);
    assert (_1 == "Hello World");
    assert (_2 == " World");
    assert (_3 == " @@@");
    assert (_4 == " !!!");
};
let test36 = _test36();

let case37 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test37 = () : unit => {
    let _1 = case37(1);
    let _2 = case37(2);
    let _3 = case37(3);
    let _4 = case37(4);
    assert (_1 == "Hello end");
    assert (_2 == " World @@@ !!!");
    assert (_3 == " @@@ !!!");
    assert (_4 == " !!!");
};
let test37 = _test37();

let case38 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test38 = () : unit => {
    let _1 = case38(1);
    let _2 = case38(2);
    let _3 = case38(3);
    let _4 = case38(4);
    assert (_1 == "Hello end");
    assert (_2 == " World @@@ end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!!");
};
let test38 = _test38();

let case39 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test39 = () : unit => {
    let _1 = case39(1);
    let _2 = case39(2);
    let _3 = case39(3);
    let _4 = case39(4);
    assert (_1 == "Hello end");
    assert (_2 == " World @@@");
    assert (_3 == " @@@");
    assert (_4 == " !!!");
};
let test39 = _test39();

let case40 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test40 = () : unit => {
    let _1 = case40(1);
    let _2 = case40(2);
    let _3 = case40(3);
    let _4 = case40(4);
    assert (_1 == "Hello end");
    assert (_2 == " World end");
    assert (_3 == " @@@ !!!");
    assert (_4 == " !!!");
};
let test40 = _test40();

let case41 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test41 = () : unit => {
    let _1 = case41(1);
    let _2 = case41(2);
    let _3 = case41(3);
    let _4 = case41(4);
    assert (_1 == "Hello end");
    assert (_2 == " World end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!!");
};
let test41 = _test41();

let case42 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test42 = () : unit => {
    let _1 = case42(1);
    let _2 = case42(2);
    let _3 = case42(3);
    let _4 = case42(4);
    assert (_1 == "Hello end");
    assert (_2 == " World end");
    assert (_3 == " @@@");
    assert (_4 == " !!!");
};
let test42 = _test42();

let case43 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test43 = () : unit => {
    let _1 = case43(1);
    let _2 = case43(2);
    let _3 = case43(3);
    let _4 = case43(4);
    assert (_1 == "Hello end");
    assert (_2 == " World");
    assert (_3 == " @@@ !!!");
    assert (_4 == " !!!");
};
let test43 = _test43();

let case44 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test44 = () : unit => {
    let _1 = case44(1);
    let _2 = case44(2);
    let _3 = case44(3);
    let _4 = case44(4);
    assert (_1 == "Hello end");
    assert (_2 == " World");
    assert (_3 == " @@@ end");
    assert (_4 == " !!!");
};
let test44 = _test44();

let case45 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            break;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test45 = () : unit => {
    let _1 = case45(1);
    let _2 = case45(2);
    let _3 = case45(3);
    let _4 = case45(4);
    assert (_1 == "Hello end");
    assert (_2 == " World");
    assert (_3 == " @@@");
    assert (_4 == " !!!");
};
let test45 = _test45();

let case46 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test46 = () : unit => {
    let _1 = case46(1);
    let _2 = case46(2);
    let _3 = case46(3);
    let _4 = case46(4);
    assert (_1 == "Hello");
    assert (_2 == " World @@@ !!!");
    assert (_3 == " @@@ !!!");
    assert (_4 == " !!!");
};
let test46 = _test46();

let case47 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test47 = () : unit => {
    let _1 = case47(1);
    let _2 = case47(2);
    let _3 = case47(3);
    let _4 = case47(4);
    assert (_1 == "Hello");
    assert (_2 == " World @@@ end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!!");
};
let test47 = _test47();

let case48 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test48 = () : unit => {
    let _1 = case48(1);
    let _2 = case48(2);
    let _3 = case48(3);
    let _4 = case48(4);
    assert (_1 == "Hello");
    assert (_2 == " World @@@");
    assert (_3 == " @@@");
    assert (_4 == " !!!");
};
let test48 = _test48();

let case49 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test49 = () : unit => {
    let _1 = case49(1);
    let _2 = case49(2);
    let _3 = case49(3);
    let _4 = case49(4);
    assert (_1 == "Hello");
    assert (_2 == " World end");
    assert (_3 == " @@@ !!!");
    assert (_4 == " !!!");
};
let test49 = _test49();

let case50 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test50 = () : unit => {
    let _1 = case50(1);
    let _2 = case50(2);
    let _3 = case50(3);
    let _4 = case50(4);
    assert (_1 == "Hello");
    assert (_2 == " World end");
    assert (_3 == " @@@ end");
    assert (_4 == " !!!");
};
let test50 = _test50();

let case51 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            break;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test51 = () : unit => {
    let _1 = case51(1);
    let _2 = case51(2);
    let _3 = case51(3);
    let _4 = case51(4);
    assert (_1 == "Hello");
    assert (_2 == " World end");
    assert (_3 == " @@@");
    assert (_4 == " !!!");
};
let test51 = _test51();

let case52 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test52 = () : unit => {
    let _1 = case52(1);
    let _2 = case52(2);
    let _3 = case52(3);
    let _4 = case52(4);
    assert (_1 == "Hello");
    assert (_2 == " World");
    assert (_3 == " @@@ !!!");
    assert (_4 == " !!!");
};
let test52 = _test52();

let case53 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            break;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test53 = () : unit => {
    let _1 = case53(1);
    let _2 = case53(2);
    let _3 = case53(3);
    let _4 = case53(4);
    assert (_1 == "Hello");
    assert (_2 == " World");
    assert (_3 == " @@@ end");
    assert (_4 == " !!!");
};
let test53 = _test53();

let case54 = (n : int) : string => {
    let output = "";
    switch (n) {
        case 1:
            output = output + "Hello";
            return output;
        case 2:
            output = output + " World";
            return output;
        case 3:
            output = output + " @@@";
            return output;
        default:
            output = output + " !!!";
            return output;
    };
    output = output + " end";
    return output;
};
let _test54 = () : unit => {
    let _1 = case54(1);
    let _2 = case54(2);
    let _3 = case54(3);
    let _4 = case54(4);
    assert (_1 == "Hello");
    assert (_2 == " World");
    assert (_3 == " @@@");
    assert (_4 == " !!!");
};
let test54 = _test54();
