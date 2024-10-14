type ('k, 'v) tree =
  | Leaf
  | Node of 'k * 'v * ('k, 'v) tree * ('k, 'v) tree

let empty =
  Leaf

(* 
let rec dump show_key show_value tree =
  match tree with
  | Leaf -> Printf.printf "Leaf"
  | Node (key, value, left, right) ->
    Printf.printf "Node (%s, %s, " (show_key key) (show_value value);
    dump show_key show_value left;
    Printf.printf ", ";
    dump show_key show_value right;
    Printf.printf ")" *)

let rec insert compare key value tree =
  match tree with
  | Leaf -> Node (key, value, Leaf, Leaf)
  | Node (key', value', left, right) ->
    match compare key key' with
    | 0 -> Node (key, value, left, right)
    | x when x < 0 ->
      Node (key', value', insert compare key value left, right)
    | _ ->
      Node (key', value', left, insert compare key value right)

(* 
(* не получилось проверить (не знаю, как распечатать option), 
   но она хвостово-рекурсивная, т.к. вызов осуществляется последним! *)
let rec find compare key tree =
  match tree with 
  | Leaf -> None
  | Node (key_of_tree, value, left, right) ->
    match (compare key key_of_tree) with 
    | 0 -> Some value
    | x when x < 0 -> find compare key left
    | _ -> find compare key right *)

(* не хвостово-рекурсивная, т.к. вызывается внутри возвращаемого выражения*)
let rec map replace tree =
  match tree with
  | Leaf -> Leaf
  | Node (key, value, left, right) ->
    Node (key, replace value, map replace left, map replace right)

let replace_even x =
  if (x mod 2 = 0) then
    (x-1)
  else
    x

(* не является хвостово-рекурсивной, т.к. вызов не осуществляется последним*)
let rec exists to_bool tree =
  match tree with
  | Leaf -> false
  | Node (_, value, left, right) ->
    match (to_bool value) with
    | true -> true
    | false -> (exists to_bool left) || (exists to_bool right)

let even x =
  x mod 2 = 0

let () =
  (* let dump' = dump (Printf.sprintf "%s") (Printf.sprintf "%i") in *)
  let insert' = insert String.compare in
  (* let find' = insert String.compare in *)

  empty
  |> insert' "abc" 10
  |> insert' "def" (-1)
  |> insert' "aardvark" 1337
  |> map replace_even
  |> exists even
  |> Printf.printf "%b\n";
  (* |> dump'; *)

print_newline ();