type myt = Cons((int, int)) | Nil(unit);

type myr = {a: int, b: nat, c: string };

type myd = One(myt) | Two(myr);

let t1 = 
  ((x: (myt, myt)) => 
     let fr = ((x: myt) => 1);
     let fl = ((x: myt) => 2);
     switch x{
     | Nil, ys => fr(ys)
     | xs, Nil => fl(xs)
     | Cons (a, b) , Cons (c, d) => a + b +c +d
     });

// let t2 = 
//   ((x: myt) => 
//      ((y: myt) =>
//         switch x {
//         | Nil => {
//             switch y {
//             | Nil => 1
//             | Cons (a, b) =>
//                 let a = "a";
//                 int(Bytes.length(a)) + b
//             }
//         }
//         | Cons (a, b) =>
//             let old_b = b;
//             let b = 
//               switch y{
//               | Nil => {
//                   let f = ((b: int) : int => a+b) ;
//                   f(b+1)
//               }
//               | _ => a + b
//               };
//             a + old_b + b
//         }));

// REASONLIGO LEFTOVER (linearity check triggers when it should not)
// let t3 = 
//   ((x: myd) => 
//      switch x{
//      | One (Nil) => 1
//      | One x => (
//          switch x {
//          | Nil => 2
//          | Cons (a, b) => a + b
//          }
//      )
//      | Two {a , b , c } => a + int (b) + int(Bytes.length(c))
//      });

// REASONLIGO LEFTOVER (linearity check triggers when it should not)
// let t2_3 = 
//   ((x: myt) => 
//      ((y: myt) => 
//         ((x2: myd) => 
//            let t2 = 
//              switch x {
//              | Nil => {
//                  switch y {
//                  | Nil => 1
//                  | Cons(a, b) =>
//                      let a = "a";
//                      int(Bytes.length(a)) + b
//                  }
//              }
//              | Cons (a, b) =>
//                  let old_b = b;
//                  let b = 
//                    switch y{
//                    | Nil =>
//                        let f = ((b: int) => a + b);
//                        f((ADD((b), (1))))
//                    | Cons (a, b) => a + b
//                    };
//                  a + old_b + b
//              };
//            let t3 = 
//              switch (x2) {
//              | One Nil => 1
//              | One x => {
//                  switch x{
//                  | Nil => 2
//                  | Cons (a, b) => a + b
//                  }
//              }
//              | Two {a : a , b : b , c : c } => a + b + int(Bytes.length(c))
//              };
//            t2 + t3 )));

// REASONLIGO LEFTOVER (linearity check triggers when it should not)
// let t4 = 
//   ((x: myd) => 
//      ((y: myd) => 
//         switch (x, y) {
//         | a, One x => 1
//         | One Nil, y => 2
//         | One (Cons a, b), y => (ADD((a), (b)))
//         | Two {a , b , c}, Two{a : aa, b : _, c : cc } =>
//           a + int(b) + int (Bytes.length (c)) + aa + int (Bytes.length (cc))
//         }));

// REASONLIGO LEFTOVER
// let t5 = (x: int) => {
//   switch (x, ()) {
//   | (a, ()) => a
//   };
// };

// REASONLIGO LEFTOVER
// let t6 = 
//   ((x: int) =>
//      switch (x, ()) {
//      | (_, _) => 2
//      };
//   )

let t7 = 
  ((x: option(int)) => 
     switch x{
     | Some x => x
     | None => 1
     });

let t8 = 
  ((x: option((int, int))) => 
     ((y: int) => 
        switch (x, y) {
        | None, x => x
        | Some x, y => x + y
        }));

let t9 = 
  ((x: option(int)) => 
     ((y: option(int)) => 
        switch (x, y) {
        | None, ys => 1
        | xs, None => 2
        | Some a, Some b => a+b
        }));

type optioni = option(int);

type myti = Consi(optioni) | Nili(unit);

let fl = ((x: myti) => 1);

let fo = ((x: optioni) => 2);

let t10 = 
  ((x: myti) => 
     ((y: myti) => 
        switch (x, y) {
        | Nili, ys => fl(ys)
        | xs, Nili => fl(xs)
        | Consi(None), Consi(Some b) =>
          let b = 1;
          b
        | Consi a, Consi b => fo(a) + fo(b)
        }));

let t11 = 
  ((x: myti) => 
     ((y: myti) => 
        switch (x, y) {
        | Nili, ys => fl(ys)
        | xs, Nili => fl(xs)
        | Consi(Some a), Consi(Some b) =>
            let a = 1;
            a+b
        | Consi a, Consi b => {
            switch a {
            | None => fo(a) + fo(b)
            | Some a => a
            }
        }
        }));

// REASONLIGO LEFTOVER (nested list patterns)
// let t12 = 
//   ((x: list(int)) => 
//      switch x{
//      | [] => 0
//      | [hd, ...[]] => hd
//      | [hd, ...[hd2, ...[]]] => hd + hd2
//      | [hd, ...[hd2, ...[hd3, ...[]]]] => hd + hd2 + hd3
//      | [hd, ...tl] => (NEG((1)))
//      });

type recordi = {a: option(list(int)), b: list(int) };

let none_a = {a: (None : option(list(int))), b: [42] };

let some_a = {a: (Some(([1, 2, 3, 4]))), b: [42] };

let a_empty_b_not = {
  a: (Some((([] : list(int))))),
  b: [111]
};

let b_empty_a_not = {
  a: (Some(([222]))),
  b: ([] : list(int))
};

// REASONLIGO LEFTOVER (nested list patterns)
// let t13 = 
//   ((x: recordi) => 
//      ((y: recordi) => 
//         switch (x, y) {
//         | {a : None, b : _ }, {a : _, b : _ } => -1
//         | { a : _, b : _ }, {a : Some [], b : [hd, ...tl] } =>
//             hd
//         | { a : _, b = _ }, {a : Some [hd, ...tl], b : [] } =>
//             hd
//         | { a : Some a, b : _}, _ => int(Bytes.length(a))
//         }));
