open Interval

assert(iAdd((1,3),(2,5)) == (3,8))
assert(iSub((1,3),(2,5)) == (-4,1))

assert(iMul((1,2),(1,2)) == (1,4))
assert(iMul((1,2),(-2,-1)) == (-4,-1))
assert(iMul((-2,-1),(1,2)) == (-4,-1))
assert(iMul((-2,-1),(-2,-1)) == (1,4))
assert(iMul((-2,3),(1,2)) == (-4,6))
assert(iMul((-2,3),(-2,-1)) == (-6,4))
assert(iMul((-2,3),(-2,3)) == (-6,9))
assert(iMul((-3,2),(-3,2)) == (-6,9))

assert(iDiv((1,2),(1,2)) == (1,2))
assert(iDiv((1,2),(1,3)) == (1,2))
assert(iDiv((1,2),(2,3)) == (1,1))
try { let _ = iDiv((1,2),(4,5)); assert(false) } catch { | BadDiv => ()}
assert(iDiv((1,2),(-2,-1)) == (-2,-1))
assert(iDiv((1,2),(-3,-1)) == (-2,-1))
assert(iDiv((1,2),(-3,-2)) == (-1,-1))
try { let _ = iDiv((1,2),(-5,-4)); assert(false) } catch { | BadDiv => ()}
assert(iDiv((-2,-1),(1,2)) == (-2,-1))
assert(iDiv((-2,-1),(1,3)) == (-2,-1))
assert(iDiv((-2,-1),(2,3)) == (-1,-1))
try { let _ = iDiv((-2,-1),(4,5)); assert(false) } catch { | BadDiv => ()}
assert(iDiv((-2,-1),(-2,-1)) == (1,2))
assert(iDiv((-2,-1),(-3,-1)) == (1,2))
assert(iDiv((-2,-1),(-3,-2)) == (1,1))
try { let _ = iDiv((-2,-1),(-5,-4)); assert(false) } catch { | BadDiv => ()}
assert(iDiv((-2,2),(-2,2)) == (-2,2))
assert(iDiv((-2,3),(-2,2)) == (-3,3))
