module Js = {
  let log = _ => 1;
};

let tup = (11, 12);
let (a, b) = tup;
Js.log(a);

let someArr = [|1, 2, 3|];

Js.log(someArr[0]);

let s = "abc";

Js.log(s.[1]);

let add = (a, b) => a + b;

let add5 = add(5);

Js.log(add5(10));

let add = (~a, ~b) => a + b;

let add10 = add(~a=10);

Js.log(add10(~b=10));

let rec fib = n =>
  switch (n <= 1) {
  | true => 1
  | _ => fib(n - 2) + fib(n - 1)
  };

Js.log(fib(10));

let l = List.mapi((i, x) => x * (i + 1), [12, 5, 8, 4]);

Js.log(Array.of_list(l));
