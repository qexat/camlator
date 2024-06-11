type precedence =
  | SUM
  | PRODUCT
  | EXPONENT
  | PREFIX
[@@deriving enum show]

let precedence_to_enum precedence =
  match precedence with
  | SUM -> 1
  | PRODUCT -> 2
  | EXPONENT -> 3
  | PREFIX -> 4
;;
