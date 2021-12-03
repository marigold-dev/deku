type abc = (int, int, int);

let projection_abc = (tpl: abc): int => tpl[1];

let modify_abc = (tpl : abc) : abc =>
   (tpl[0], 2048,tpl[2]);

type foobar = (int, int);

let fb: foobar = (0, 0);

let projection = (tpl: foobar): int => tpl[0] + tpl[1];

type big_tuple = (int, int, int, int, int, int, int, int, int, int, int, int);

let br : big_tuple = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

let update = (tpl : big_tuple) : big_tuple =>
   (tpl[0],tpl[1],tpl[2],tpl[3],tpl[4],tpl[5],tpl[6],tpl[7],tpl[8],tpl[9],tpl[10],2048)