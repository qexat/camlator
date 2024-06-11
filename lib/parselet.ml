type 'parser atom_parselet_func = parser:'parser -> token:Token.token -> Ast.expr
type 'parser prefix_parselet_func = parser:'parser -> token:Token.token -> Ast.expr

type 'parser infix_parselet_func =
  parser:'parser -> left:Ast.expr -> token:Token.token -> Ast.expr

module type ParserType = sig
  type t

  val parse_expression : precedence:int option -> t -> Ast.expr
end

module type InfixParselet = functor (Parser : ParserType) -> sig
  val precedence : int
  val is_right_associative : bool
  val parse : parser:Parser.t -> token:Token.token -> Ast.expr
end
