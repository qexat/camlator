type binary_op_token_type =
  | MINUS
  | PLUS
  | SLASH
  | STAR
[@@deriving show]

type token_type =
  | INTEGER
  | INVALID
  | BINARY_OP of binary_op_token_type
  | EOF
[@@deriving show]

let unwrap_binary_op = function
  | BINARY_OP op -> op
  | _ -> failwith "unwrap failed: expected binary op"
;;
