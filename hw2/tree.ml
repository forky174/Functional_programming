type ('k, 'v) tree =
  | Leaf
  | Node of 'k * 'v * ('k, 'v) tree * ('k, 'v) tree

let empty =
  Leaf

(* let rec dump show_key show_value tree =
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

let rec fold' f init tree k =
  match tree with
  | Leaf -> k init
  | Node (key, value, left, right) ->
    fold' f init right (fun rec_2 ->
    fold' f rec_2 left ( fun rec_1 ->
    k (f key value rec_1)))

let fold f init tree = 
  fold' f init tree (fun x -> x)

let f key value init =
  if (key.[0] = 'a' || key.[2] = 'g') then init + value
  else init

(* let rec all' p tree yes no =
  match tree with
  | Leaf -> yes ()
  | Node (_, value, left, right) ->
    if p value then (all' p left (fun () -> all' p right yes no) no)
    else no ()

let all p tree =
  all' p tree (fun () -> true) (fun () -> false) 

let even x =
  x mod 2 = 0 *)

let () =
  (* let dump' = dump (Printf.sprintf "%s") (Printf.sprintf "%i") in *)
  let insert' = insert String.compare in

  empty
  |> insert' "abc" (6)
  |> insert' "dfg" (15)
  |> insert' "aaa" (10)
  |> fold f 0
  |> Printf.printf "%i\n";
  (* |> all even
  |> Printf.printf "%b\n"; *)
  (* |> dump'; *)

print_newline ();