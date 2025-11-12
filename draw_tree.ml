(* draw_tree.ml
 * Get a string representation of a binary tree
 *
 * Programmer: Amit Rubin, 2025
 *)

(* The underlying algorithm is straightforward: For a given inner node 
 * 1. Recursively find the representation of the left subtree
 * 2. Recursively find the representation of the right subtree
 * 3. shift right the representation of the right tree by the  width 
 * of the representation of the left tree.
 *)

module DrawTree : sig
  type str_tree = Leaf of string | Node of (string * str_tree * str_tree)

  val convert_to_str_tree : 
    is_node: ('t -> bool) -> 
    get_left: ('t -> 't) -> 
    get_right: ('t -> 't) -> 
    get_data_str: ('t -> string) -> 
    t: 't -> str_tree
    
  val str_tree_sprintf : str_tree -> string

  val str_tree_printf : str_tree -> unit

  val example_tree : str_tree
end = struct
  type str_tree = Leaf of string | Node of (string * str_tree * str_tree)
  type draw_tree = Dleaf of {s_offset: int; s: string; am_left_child: bool}
                 | Dnode of {s_offset: int;
                             s: string;
                             left: draw_tree;
                             right: draw_tree;
                             am_left_child: bool}

  let string_length = String.length

  let rec convert_to_str_tree ~is_node ~get_left ~get_right ~get_data_str ~t  =
    if is_node t
    then Node (get_data_str t,
               convert_to_str_tree ~is_node ~get_left ~get_right ~get_data_str ~t:(get_left t),
               convert_to_str_tree ~is_node ~get_left ~get_right ~get_data_str ~t:(get_right t))
    else Leaf (get_data_str t)

  let rec str_tree_to_draw_tree (t : str_tree) = (* the heavy lifting function*)
    let seperator_size = 2 in
    let rec shift_right (t: draw_tree) (offset : int) : draw_tree =
      match t with
      | Dleaf {s_offset=i; s; am_left_child} ->
        Dleaf {s_offset=i+offset;
               s=s;
               am_left_child = am_left_child}
      | Dnode {s_offset=i; s; left=t_l; right=t_r; am_left_child} ->
        Dnode {s_offset = i + offset;
               s = s;
               left = shift_right t_l offset;
               right = shift_right t_r offset;
               am_left_child = am_left_child} in

    let rec find_righttmost_size = function
      | Dleaf {s_offset=i; s; am_left_child} -> i + (string_length s)
      | Dnode {s_offset=i; s; left=t_l; right=t_r; am_left_child} ->
        let a = find_righttmost_size t_l in
        let b = find_righttmost_size t_r in
        max (i + (string_length s)) (max a b) in

    let rec run (t : str_tree) (am_left_child : bool) = 
      match t with
      | Leaf s -> Dleaf {s_offset = 0; s = s; am_left_child = am_left_child}
      | Node (s, t_left, t_right) ->
        let draw_tree_left = run t_left true in
        let draw_tree_right = run t_right false in
        let rightmost_size_left = find_righttmost_size draw_tree_left in
        let shift_right_by_offset, start_s_offset =
          if rightmost_size_left + (seperator_size/2) < ((string_length s) / 2) 
          then (string_length s, 0)
          else (seperator_size + rightmost_size_left,
                rightmost_size_left + (seperator_size/2) - ((string_length s) / 2)) in
        let draw_tree_right =
          shift_right
            draw_tree_right
            (shift_right_by_offset + 1) in 
        Dnode {s_offset=start_s_offset; s=s;
               left=draw_tree_left;
               right=draw_tree_right;
               am_left_child=am_left_child} in
    run t true

  let tree_bfs (is_node: 't -> bool) (get_left: 't -> 't) (get_right: 't -> 't)
      (get_data_str: 't -> 'data) (t: 't) =
    let rec run next_nodes acc =
      if next_nodes = []
      then acc
      else
        let data_to_append = List.map get_data_str next_nodes in
        let new_next_nodes =
          List.concat (List.map (fun t ->
              if is_node t then [get_left t; get_right t] else []) next_nodes) in
        run new_next_nodes (List.append acc [data_to_append]) in
    run [t] []

  let draw_tree_bfs =
    let draw_tree_is_node = function | Dleaf _ -> false | _ -> true in
    let draw_tree_get_left = function | Dnode {left}  -> left
                                      | Dleaf _ -> raise (Failure "str_tree_get_left called on a leaf") in
    let draw_tree_get_right = function | Dnode {right}  -> right
                                       | Dleaf _ -> raise (Failure "str_tree_get_right called on a leaf") in
    let draw_tree_get_data_str = function | Dnode {s_offset; s; am_left_child} -> (s_offset ,s, am_left_child)
                                          | Dleaf {s_offset=i; s; am_left_child} -> (i,s, am_left_child) in
    tree_bfs draw_tree_is_node draw_tree_get_left draw_tree_get_right draw_tree_get_data_str

  let draw_tree_to_string_list t =
    let bfs_order = draw_tree_bfs t in
    let lines =
      List.map
        (fun l ->
           List.fold_left
             (fun acc (i, s, _)  ->
                let diff = i-(string_length acc) in
                let diff_string = String.make diff ' ' in
                acc ^ diff_string ^ s
             )
             ""
             l)
        bfs_order in
    let parent_lines =
      List.map
        (fun l ->
           List.fold_left
             (fun acc (i, s, am_left_child)  ->
                let parent_symbol = if am_left_child then "/" else " \\" in
                let diff = ((string_length s) / 2) + i - (string_length acc) in
                let diff = if am_left_child && (string_length s) mod 2 = 0 then diff else diff -1 in
                let diff = if am_left_child && (string_length s) mod 2 = 1 then diff + 1 else diff in
                let diff = max diff 0 in
                let diff_string = String.make diff ' ' in
                acc ^ diff_string ^ parent_symbol
             )
             ""
             l)
        bfs_order in
    let rec mix_lines l1 l2 =
      match l1, l2 with
      | [], [] -> []
      | hd1 :: tl1, hd2 :: tl2 -> hd1 :: hd2 :: (mix_lines tl1 tl2)
      | _ -> raise (Failure "should not happen in draw_tree_to_string_list")
    in
    match mix_lines parent_lines lines with
    | [] -> []
    | hd :: tl -> tl

  let str_tree_sprintf t =
    String.concat "\n" (draw_tree_to_string_list
                          (str_tree_to_draw_tree t))
    ^ "\n";;

  let str_tree_printf t =
    print_string ((String.concat "\n" (draw_tree_to_string_list (str_tree_to_draw_tree t))) ^ "\n");;

  let example_tree =
    (Node
       ("a",
        Node
          ("bb",
           Node
             ("dddd", Node ("hhhh", Leaf "1111", Leaf "2222"),
              Node ("iiii", Leaf "3", Leaf "44444444")),
           Node
             ("eeee", Leaf "jjjj",
              Node ("kkkk", Leaf "5", Leaf "6666666666666666666666"))),
        Node
          ("cccc",
           Node
             ("ffffff", Node ("llll", Leaf "AAAA", Leaf "BBBB"),
              Node ("mmmm", Leaf "CCCC", Leaf "DDDDDDDDDDDDDDDDDDDDDDDD")),
           Node
             ("gggggggggggg", Node ("nnnn", Leaf "EE", Leaf "FFFF"),
              Node ("ooo", Leaf "G", Leaf "H")))))
end
