// Interval arithmetic

exception BadDiv

let iAdd = ((a,b), (c,d)) => (a+c,b+d)

let iSub = ((a,b), (c,d)) => (a-d,b-c)

let iUni = ((a,b), (c,d)) => (min(a,c),max(b,d))

let iOpp = ((a,b))        => (-b,-a)

// c >= 0
let iMulPos = ((a,b), (c,d)) => (if a < 0 { a*d } else { a*c }, if b < 0 { b*c } else { b*d })
// d <= 0
let iMulNeg = ((a,b), (c,d)) => (if b < 0 { b*d } else { b*c }, if a < 0 { a*c } else { a*d })
let iMul = (i, (c,d) as j) => if c >= 0 { iMulPos(i,j) }
                              else if d <= 0 { iMulNeg(i,j) }
	                      else { iUni(iMulPos(i,(0,d)),iMulNeg(i,(c,0))) }

// rounding of division
let divUp = (a,b) => { let r = a/b; if (b > 0 && r*b < a) || (b < 0 && r*b > a) { r + 1 } else { r }}
let divDw = (a,b) => { let r = a/b; if (b > 0 && r*b > a) || (b < 0 && r*b < a) { r - 1 } else { r }}

// c > 0
let iDivPos = ((a,b), (c,d)) => (if a < 0 { divUp(a,c) } else { divUp(a,d) },
                                  if b < 0 { divDw(b,d) } else { divDw(b,c) })
// d < 0
let iDivNeg = ((a,b), (c,d)) => ( if b < 0 { divUp(b,c) } else { divUp(b,d) },
                                  if a < 0 { divDw(a,d) } else { divDw(a,c) })

// check for empyness
let chk = ((a,b) as i) => if b < a { raise(BadDiv) } else { i }

let iDiv = (i,(c,d)) => chk(if c == 0 && d == 0 { raise(BadDiv) }
                            else if c >= 0 { iDivPos(i,(max(1,c),d)) }
 	                    else if d <= 0 { iDivNeg(i,(c,min(-1,d))) }
			    else { iUni(iDivNeg(i,(c,-1)),iDivPos(i,(1,d))) })
