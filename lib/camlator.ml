module Tree = struct
  type 'typ t =
    { value : 'typ
    ; children : 'typ t list
    }
  [@@deriving show]

  let create root = { value = root; children = [] }
  let prepend_node node tree = { value = tree.value; children = node :: tree.children }
  let append_node node tree = { value = tree.value; children = tree.children @ [ node ] }
  let prepend value tree = prepend_node (create value) tree
  let append value tree = append_node (create value) tree

  let rec map func tree =
    { value = func tree.value; children = List.map (map func) tree.children }
  ;;

  let rec fold func init tree =
    func tree.value (List.fold_left func init (List.map (fold func init) tree.children))
  ;;

  let flatten tree = fold List.append [] (map (fun a -> [ a ]) tree)
  let ( => ) = prepend

  let rec from_values values =
    match values with
    | [] -> failwith "expected at least one value"
    | root :: [] -> create root
    | head :: tail ->
      let tree = create head in
      append_node (from_values tail) tree
  ;;
end
