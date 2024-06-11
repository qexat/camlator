type 'parser atom_parselet_func = parser:'parser -> token:Token.token -> Ast.expr
type 'parser prefix_parselet_func = parser:'parser -> token:Token.token -> Ast.expr

type 'parser infix_parselet_func =
  parser:'parser -> left:Ast.expr -> token:Token.token -> Ast.expr
