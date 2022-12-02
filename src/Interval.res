// Interval arithmetic
// Implement the standard operation on interval. The result of an operation
// (a,b) $op (c,d) is an interval (e,f) that contains all the value
// x $op y for x in [a,b] and y in [c,d]

type t = (int, int)

let iAdd : (t, t) => t
         = ((a,b), (c,d)) => (a+c,b+d)

let iSub : (t, t) => t
         = ((a,b), (c,d)) => (a-d,b-c)

let iOpp : t => t
         = ((a,b))        => (-b,-a)

// Union of intervals need for multiplication and division
let iUni : (t, t) => t
         = ((a,b), (c,d)) => (min(a,c),max(b,d))

// multiplication when right interval is positive
let iMulPos = ((a,b), (c,d)) => (if a < 0 { a*d } else { a*c },
                                 if b < 0 { b*c } else { b*d })
// multiplication when right interval is negative
let iMulNeg = ((a,b), (c,d)) => (if b < 0 { b*d } else { b*c },
                                 if a < 0 { a*c } else { a*d })
// final multiplication
let iMul : (t, t) => t
         = (i, (c,d) as j) => if c >= 0 { iMulPos(i,j) }
                              else if d <= 0 { iMulNeg(i,j) }
	                      else { iUni(iMulPos(i,(0,d)),iMulNeg(i,(c,0))) }

// exception raised when division are not exact or when dividing by zero
exception BadDiv

// rounding of division up and down. Tricky because division of negative is not
// well specified.
let divUp = (a,b) => { let r = a/b; if (b > 0 && r*b < a) || (b < 0 && r*b > a) { r + 1 } else { r }}
let divDw = (a,b) => { let r = a/b; if (b > 0 && r*b > a) || (b < 0 && r*b < a) { r - 1 } else { r }}

// division when right interval is positive
let iDivPos = ((a,b), (c,d)) => (if a < 0 { divUp(a,c) } else { divUp(a,d) },
                                  if b < 0 { divDw(b,d) } else { divDw(b,c) })
// division when right interval is negative
let iDivNeg = ((a,b), (c,d)) => ( if b < 0 { divUp(b,c) } else { divUp(b,d) },
                                  if a < 0 { divDw(a,d) } else { divDw(a,c) })

// check for emptyness of interval which means that division if never exact
// for any value in the given intervals
let chk = ((a,b) as i) => if b < a { raise(BadDiv) } else { i }

// final division
let iDiv : (t, t) => t
         = (i,(c,d)) => chk(if c == 0 && d == 0 { raise(BadDiv) }
                            else if c >= 0 { iDivPos(i,(max(1,c),d)) }
 	                    else if d <= 0 { iDivNeg(i,(c,min(-1,d))) }
			    else { iUni(iDivNeg(i,(c,-1)),iDivPos(i,(1,d))) })
