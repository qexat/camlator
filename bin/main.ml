(* TODO: fix the parser to take precedence in account *)

let source = "3 + 5"
let tok = new Camlator.Tokenizer.tokenizer source
let tokens = tok#run ()

let () =
  Camlator.Parser.register_atom_parselet
    Camlator.Token_type.INTEGER
    Camlator.Parselet_impls.literal_parselet;
  Camlator.Parser.register_infix_parselet
    (Camlator.Token_type.BINARY_OP Camlator.Token_type.PLUS)
    (Camlator.Parselet_impls.binary_op_parselet
       ~precedence:Camlator.Precedence.SUM
       ~is_right_associative:false);
  Camlator.Parser.register_infix_parselet
    (Camlator.Token_type.BINARY_OP Camlator.Token_type.MINUS)
    (Camlator.Parselet_impls.binary_op_parselet
       ~precedence:Camlator.Precedence.SUM
       ~is_right_associative:false);
  Camlator.Parser.register_infix_parselet
    (Camlator.Token_type.BINARY_OP Camlator.Token_type.STAR)
    (Camlator.Parselet_impls.binary_op_parselet
       ~precedence:Camlator.Precedence.PRODUCT
       ~is_right_associative:false);
  Camlator.Parser.register_infix_parselet
    (Camlator.Token_type.BINARY_OP Camlator.Token_type.SLASH)
    (Camlator.Parselet_impls.binary_op_parselet
       ~precedence:Camlator.Precedence.PRODUCT
       ~is_right_associative:false)
;;

let parser = Camlator.Parser.create tokens
let expr = Camlator.Parser.run parser

let () =
  print_int (Camlator.Eval.eval expr);
  print_newline ()
;;
