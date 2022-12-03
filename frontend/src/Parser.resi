open Expr

// parses a string using the usual priorities and ignoring blanks.
// raise Parse_error(pos) when parsing fails after pos characters
exception Parse_error(int)
let parse : string => expr