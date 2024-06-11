type token =
  { typ : Token_type.token_type
  ; lexeme : string
  ; startpos : int
  ; endpos : int
  }
[@@deriving show]

(* let print_token (token : token) = Printf.printf "%s\n" (show_token token) *)
(* let print_tokens (tokens : token list) = List.iter print_token tokens *)
