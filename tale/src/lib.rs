pub mod compiler;
pub mod parser;
pub mod vm;

/*
// expr
term = _{ num | ident | string | bool | "(" ~ expr ~ ")" }
terms = { term+ }

compare_op = _{ "==" | "!=" | ">" | "<" | ">=" | "<=" }
compare_expr = { expr ~ compare_op ~ expr }

arith_op = _{ "+" | "-" | "*" | "/" }
arith_expr = { expr ~ arith_op ~ expr }

neg_op = _{ "!" }
neg_expr = { neg_op ~ expr }

and_op = _{ "&&" | "and }
or_op = _{ "||" | "or" }
bool_op = _{ and_op | or_op }
bool_expr = { (expr ~ bool_op ~ expr) | neg_expr }

binary_expr = { bool_expr | arith_expr | compare_expr }
unary_expr = { neg_expr }

expr = { binary_expr | unary_expr | term }

*/
