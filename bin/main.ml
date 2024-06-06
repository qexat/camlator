type binary_op_token_type =
  | MINUS
  | PLUS
  | SLASH
  | STAR
[@@deriving show]

type token_type =
  | INTEGER
  | INVALID
  | EOF
  | BINARY_OP of binary_op_token_type
[@@deriving show]

type token =
  { typ : token_type
  ; lexeme : string
  ; startpos : int
  ; endpos : int
  }
[@@deriving show]

class tokenizer (source : string) =
  object (self)
    val mutable start = 0
    val mutable current = 0
    val mutable tokens : token list = []
    method is_at_end ?(n = 0) () = current + n >= String.length source

    method peek ?(n = 0) () =
      if self#is_at_end ?n:(Some n) () then '\x00' else source.[current]

    method advance ?(steps = 1) () = current <- current + steps
    method sync_head () = start <- current

    method consume () =
      let character = self#peek () in
      self#advance ();
      character

    method make_integer () =
      let chr = self#peek () in
      match chr with
      | '0' .. '9' ->
        self#advance ();
        self#make_integer ()
      | _ -> INTEGER

    method scan_token () =
      match self#consume () with
      | ' ' | '\r' | '\t' ->
        self#sync_head ();
        self#scan_token ()
      | '-' -> BINARY_OP MINUS
      | '+' -> BINARY_OP PLUS
      | '/' -> BINARY_OP SLASH
      | '*' -> BINARY_OP STAR
      | '0' .. '9' -> self#make_integer ()
      | _ -> INVALID

    method get_lexeme = String.sub source start (current - start)

    method build_token typ =
      { typ; lexeme = self#get_lexeme; startpos = start; endpos = current }

    method run =
      if self#is_at_end ()
      then (
        self#sync_head ();
        tokens <- self#build_token EOF :: tokens;
        List.rev tokens)
      else (
        self#sync_head ();
        tokens <- self#build_token (self#scan_token ()) :: tokens;
        self#run)
  end

let print_token (token : token) = Printf.printf "%s\n" (show_token token)
let print_tokens (tokens : token list) = List.iter print_token tokens
let source = "3 + 5"
let tok = new tokenizer source
let () = print_tokens tok#run

type expr =
  | BinaryOpExpr of
      { operator : binary_op_token_type
      ; left : expr
      ; right : expr
      }
  | InvalidExpr of
      { message : string
      ; token : token
      ; subexprs : expr list
      }
  | LiteralExpr of { token : token }

(* TODO: make the parser work lol *)

class parser_base (tokens : token list) =
  object (self)
    val mutable current = 0
    val mutable buffer = []
    val mutable prefix_parselets = Hashtbl.create 32
    val mutable infix_parselets = Hashtbl.create 32

    method lookahead distance =
      if distance >= List.length buffer
      then (
        buffer <- buffer @ [ List.nth tokens current ];
        self#lookahead (distance - 1))
      else List.nth buffer distance

    method consume ?expected () =
      let token = self#lookahead 0 in
      match expected with
      | Some token_type ->
        if token.typ != token_type
        then Result.error "unexpected token type"
        else self#consume ()
      | None ->
        let _ = self#lookahead 0 in
        (match buffer with
         | h :: t ->
           buffer <- t;
           Result.ok h
         | [] -> failwith "empty buffer")

    method matches expected =
      let token = self#lookahead 0 in
      if token.typ != expected
      then false
      else (
        let _ = self#consume () in
        true)

    method register_prefix token_type parselet =
      Hashtbl.add prefix_parselets token_type parselet

    method register_infix token_type parselet =
      Hashtbl.add infix_parselets token_type parselet

    method parse_expression ?(precedence = 0) () =
      match self#consume () with
      | Result.Ok token ->
        (match Hashtbl.find_opt prefix_parselets token.typ with
         | Some prefix ->
           let left = prefix#parse self token in
           ()
         | None -> failwith ("could not parse \"" ^ token.lexeme ^ "\""))
      | Result.Error message -> failwith message
  end

module type PrefixParselet = sig
  val parse : parser:parser_base -> token:token -> expr
end

module type InfixParselet = sig
  val parse : parser:parser_base -> left:expr -> token:token -> expr
end

module BinaryOpParselet : InfixParselet = struct
  let parse parser left token =
    let right = parser#parse_expression (precedence - Bool.to_int is_right_associative) in
    BinaryOpExpr { operator = token.typ; left; right }
  ;;
end

let binary_op_of_binary_op_token_type = function
  | MINUS -> fun a b -> a - b
  | PLUS -> fun a b -> a + b
  | SLASH -> fun a b -> a / b
  | STAR -> fun a b -> a * b
;;

let rec eval = function
  | BinaryOpExpr { operator; left; right } ->
    (binary_op_of_binary_op_token_type operator) (eval left) (eval right)
  | LiteralExpr { token : token } -> int_of_string token.lexeme
  | InvalidExpr _ -> raise (Failure "invalid expression found")
;;
