let literal_parselet ~parser:_ ~token = Ast.LiteralExpr { token }

let binary_op_parselet ~precedence ~is_right_associative ~parser ~left ~token =
  let right =
    Parser.parse_expression
      parser
      ?precedence:
        (Some (Precedence.precedence_to_enum precedence - Bool.to_int is_right_associative))
  in
  Ast.BinaryOpExpr { operator = Token_type.unwrap_binary_op token.Token.typ; left; right }
;;
