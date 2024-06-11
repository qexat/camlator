type t = { tokens : Token.token list }

type parselets =
  { atom_parselets : (Token_type.token_type, t Parselet.atom_parselet_func) Hashtbl.t
  ; prefix_parselets : (Token_type.token_type, t Parselet.prefix_parselet_func) Hashtbl.t
  ; infix_parselets : (Token_type.token_type, t Parselet.infix_parselet_func) Hashtbl.t
  }

type parser_state =
  { mutable current : int
  ; mutable buffer : Token.token list
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
  | Some _parselet -> 1 (*parselet.precedence*)
  | None -> 0
;;

let rec parse_binary_expression
  parser
  (prefix : t Parselet.prefix_parselet_func)
  left
  precedence
  =
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
       Ast.InvalidExpr
         { message = "could not parse \"" ^ token.lexeme ^ "\""; token; subexprs = [] })
  | Result.Error message -> failwith message
;;

let run parser =
  reset_parser ();
  parse_expression parser
;;
