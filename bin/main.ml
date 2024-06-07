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

type 'parser prefix_parselet = parser:'parser -> token:token -> expr
type 'parser infix_parselet = parser:'parser -> left:expr -> token:token -> expr

module ParserBase = struct
  type t = { tokens : token list }

  type parselets =
    { prefix_parselets : (token_type, t prefix_parselet) Hashtbl.t
    ; infix_parselets : (token_type, t infix_parselet) Hashtbl.t
    }

  type parser_state =
    { mutable current : int
    ; mutable buffer : token list
    }

  let state = { current = 0; buffer = [] }

  let parselets =
    { prefix_parselets = Hashtbl.create 32; infix_parselets = Hashtbl.create 32 }
  ;;

  let reset_parser () =
    state.current <- 0;
    state.buffer <- [];
    Hashtbl.clear parselets.prefix_parselets;
    Hashtbl.clear parselets.infix_parselets
  ;;

  let create tokens = { tokens }

  let register_prefix_parselet token_type parselet =
    Hashtbl.add parselets.prefix_parselets token_type parselet
  ;;

  let register_infix_parselet token_type parselet =
    Hashtbl.add parselets.infix_parselets token_type parselet
  ;;

  let rec lookahead parser distance =
    if distance >= List.length state.buffer
    then (
      state.buffer <- state.buffer @ [ List.nth parser.tokens state.current ];
      lookahead parser (distance - 1))
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

  let matches parser expected =
    let token = lookahead parser 0 in
    if token.typ != expected
    then false
    else (
      let _ = consume parser () in
      true)
  ;;

  let get_precedence parser =
    match Hashtbl.find_opt parselets.infix_parselets (lookahead parser 0).typ with
    | Some _ -> 1
    | None -> 0
  ;;

  let rec parse_binary_expression parser prefix left token precedence =
    if precedence < get_precedence parser
    then (
      match consume parser () with
      | Result.Ok token ->
        let infix = Hashtbl.find parselets.infix_parselets token.typ in
        parse_binary_expression parser prefix (infix parser left token) token precedence
      | Result.Error message -> failwith message)
    else left
  ;;

  let parse_expression parser ?(precedence = 0) =
    reset_parser ();
    match consume parser () with
    | Result.Ok token ->
      (match Hashtbl.find_opt parselets.prefix_parselets token.typ with
       | Some prefix ->
         let left = prefix parser token in
         parse_binary_expression parser prefix left token precedence
       | None -> failwith ("could not parse \"" ^ token.lexeme ^ "\""))
    | Result.Error message -> failwith message
  ;;
end

let binary_op_parselet precedence =
  let is_right_associative = false in
  fun parser left token ->
    let right =
      ParserBase.parse_expression
        parser
        ?precedence:(Some (precedence - Bool.to_int is_right_associative))
    in
    BinaryOpExpr { operator = token.typ; left; right }
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
  | InvalidExpr _ -> raise (Failure "invalid expression found")
;;

let source = "3 + 5"
let tok = new tokenizer source
let () = print_tokens tok#run
