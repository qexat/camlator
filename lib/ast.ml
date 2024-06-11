type expr =
  | BinaryOpExpr of
      { operator : Token_type.binary_op_token_type
      ; left : expr
      ; right : expr
      }
  | InvalidExpr of
      { message : string
      ; token : Token.token
      ; subexprs : expr list
      }
  | LiteralExpr of { token : Token.token }
[@@deriving show]
