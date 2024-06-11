let binary_op_of_binary_op_token_type = function
  | Token_type.MINUS -> ( - )
  | Token_type.PLUS -> ( + )
  | Token_type.SLASH -> ( / )
  | Token_type.STAR -> ( * )
;;

let rec eval = function
  | Ast.BinaryOpExpr { operator; left; right } ->
    (binary_op_of_binary_op_token_type operator) (eval left) (eval right)
  | Ast.LiteralExpr { token : Token.token } -> int_of_string token.lexeme
  | Ast.InvalidExpr _ -> failwith "invalid expression found"
;;
