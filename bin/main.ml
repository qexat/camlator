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

    method get_lexeme () = String.sub source start (current - start)

    method build_token typ =
      { typ; lexeme = self#get_lexeme (); startpos = start; endpos = current }

    method run () =
      if self#is_at_end ()
      then (
        self#sync_head ();
        tokens <- self#build_token EOF :: tokens;
        List.rev tokens)
      else (
        self#sync_head ();
        tokens <- self#build_token (self#scan_token ()) :: tokens;
        self#run ())
  end

let print_token (token : token) = Printf.printf "%s\n" (show_token token)
let print_tokens (tokens : token list) = List.iter print_token tokens

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
[@@deriving show]

type precedence =
  | SUM
  | PRODUCT
  | EXPONENT
  | PREFIX
[@@deriving enum]

type 'parser atom_parselet = parser:'parser -> token:token -> expr
type 'parser prefix_parselet = parser:'parser -> token:token -> expr
type 'parser infix_parselet = parser:'parser -> left:expr -> token:token -> expr

module Parser = struct
  type t = { tokens : token list }

  type parselets =
    { atom_parselets : (token_type, t atom_parselet) Hashtbl.t
    ; prefix_parselets : (token_type, t prefix_parselet) Hashtbl.t
    ; infix_parselets : (token_type, t infix_parselet) Hashtbl.t
    }

  type parser_state =
    { mutable current : int
    ; mutable buffer : token list
    }

  let state = { current = 0; buffer = [] }

  let parselets =
    { atom_parselets = Hashtbl.create 32
    ; prefix_parselets = Hashtbl.create 32
    ; infix_parselets = Hashtbl.create 32
    }
  ;;

  let reset_parser () =
    state.current <- 0;
    state.buffer <- [];
    Hashtbl.clear parselets.prefix_parselets;
    Hashtbl.clear parselets.infix_parselets
  ;;

  let create tokens = { tokens }

  let register_atom_parselet token_type parselet =
    Hashtbl.add parselets.atom_parselets token_type parselet
  ;;

  let register_infix_parselet token_type parselet =
    Hashtbl.add parselets.infix_parselets token_type parselet
  ;;

  let rec lookahead parser distance =
    if distance >= List.length state.buffer
    then (
      state.buffer <- state.buffer @ [ List.nth parser.tokens state.current ];
      lookahead parser distance)
    else List.nth state.buffer distance
  ;;

  let rec consume parser ?expected () =
    let token = lookahead parser 0 in
    match expected with
    | Some token_type ->
      if token.typ != token_type
      then Result.error "unexpected token type"
      else consume parser ()
    | None ->
      let _ = lookahead parser 0 in
      (match state.buffer with
       | h :: t ->
         state.buffer <- t;
         Result.ok h
       | [] -> failwith "empty buffer")
  ;;

  let get_precedence parser =
    match Hashtbl.find_opt parselets.infix_parselets (lookahead parser 0).typ with
    | Some parselet -> parselet.precedence
    | None -> 0
  ;;

  let rec parse_binary_expression parser (prefix : t prefix_parselet) left precedence =
    if precedence < get_precedence parser
    then (
      match consume parser () with
      | Result.Ok token ->
        let infix = Hashtbl.find parselets.infix_parselets token.typ in
        parse_binary_expression parser prefix (infix ~parser ~left ~token) precedence
      | Result.Error message -> failwith message)
    else left
  ;;

  let parse_expression ?(precedence = 0) parser =
    match consume parser () with
    | Result.Ok token ->
      (match Hashtbl.find_opt parselets.atom_parselets token.typ with
       | Some atom ->
         let left = atom ~parser ~token in
         parse_binary_expression parser atom left precedence
       | None ->
         InvalidExpr
           { message = "could not parse \"" ^ token.lexeme ^ "\""; token; subexprs = [] })
    | Result.Error message -> failwith message
  ;;

  let run parser =
    reset_parser ();
    parse_expression parser
  ;;
end

let literal_parselet ~parser:_ ~token = LiteralExpr { token }

let binary_op_parselet ~precedence ~is_right_associative ~parser ~left ~token =
  let right =
    Parser.parse_expression
      parser
      ?precedence:
        (Some (precedence_to_enum precedence - Bool.to_int is_right_associative))
  in
  BinaryOpExpr { operator = unwrap_binary_op token.typ; left; right }
;;

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
  | InvalidExpr _ -> failwith "invalid expression found"
;;

let source = "3 + 5"
let tok = new tokenizer source
let tokens = tok#run ()

let () =
  Parser.register_atom_parselet INTEGER literal_parselet;
  Parser.register_infix_parselet
    (BINARY_OP PLUS)
    (binary_op_parselet ~precedence:SUM ~is_right_associative:false);
  Parser.register_infix_parselet
    (BINARY_OP MINUS)
    (binary_op_parselet ~precedence:SUM ~is_right_associative:false);
  Parser.register_infix_parselet
    (BINARY_OP STAR)
    (binary_op_parselet ~precedence:PRODUCT ~is_right_associative:false);
  Parser.register_infix_parselet
    (BINARY_OP SLASH)
    (binary_op_parselet ~precedence:PRODUCT ~is_right_associative:false)
;;

let parser = Parser.create tokens
let expr = Parser.run parser

let () =
  print_tokens tokens;
  print_endline (show_expr expr);
  print_int (eval expr);
  print_newline ()
;;
