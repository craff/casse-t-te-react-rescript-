// Interval arithmetic
// Implement the standard operation on interval. The result of an operation
// (a,b) $op (c,d) is an interval (e,f) that contains all the value
// x $op y for x in [a,b] and y in [c,d]

type t = (int, int)

let iAdd : (t, t) => t

let iSub : (t, t) => t

let iOpp : t => t

let iMul : (t, t) => t

// exception raised when division are not exact or when dividing by zero
exception BadDiv

let iDiv : (t, t) => t