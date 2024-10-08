(* let rec factorial n =
  match n with
  | 0 -> 1
  | n -> n * (factorial (n - 1))

let rec factorial_with_tailcall' n acc =
  match n with 
  | 0 -> acc
  | n -> factorial_with_tailcall' (n-1)  (acc*n)

let factorial_with_tailcall n =
  factorial_with_tailcall' n 1

let () =
  factorial 5
  |> Printf.printf "%i\n";
  factorial_with_tailcall 5
  |> Printf.printf "%i\n"; *)


let rec fibonacci n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | n -> (fibonacci (n-1)) + (fibonacci (n-2))

let rec fibonacci_with_tailcall' n acc1 acc2=
  match n with
  | 0 -> acc1
  | n -> fibonacci_with_tailcall' (n-1) (acc2) (acc1+acc2)

let fibonacci_with_tailcall n =
  fibonacci_with_tailcall' n 0 1

let () =
  fibonacci 6
  |> Printf.printf "%i\n";
  fibonacci_with_tailcall 6
  |> Printf.printf "%i\n";

(*
let rec filter f l =
  match l with
  | [] -> []
  | x::xs ->
    if f x then
      x::(filter f xs)
    else
      filter f xs

let rec reverse' l acc =
  match l with
  | [] -> acc
  | x::xs -> reverse' xs (x::acc)

let reverse l =
  reverse' l []

let rec filter_with_tailcall' f l acc =
  match l with
  | [] -> reverse acc
  | x::xs -> 
    match f x with
    | true -> filter_with_tailcall' f xs (x::acc)
    | false -> filter_with_tailcall' f xs acc

let filter_with_tailcall f l =
  filter_with_tailcall' f l []

let even x =
  x mod 2 = 0

let () =
  [1; 2; 5; 7; 34]
  |> filter_with_tailcall even
  |> List.iter (Printf.printf "%i "); *)

(* 
let rec fold f acc l =
  match l with
  | [] -> acc
  | x::xs -> fold f (f acc x) xs

let () =
  [1; 2; 3]
  |> fold ( + ) 0
  |> Printf.printf "%i\n"; *)

(* 
let () = 
  [1; 2; 3]
  |> fold (fun acc x -> x::acc) []
  |> List.iter (Printf.printf "%i "); *)

(* 
let rec reverse' l acc =
  match l with
  | [] -> acc
  | x::xs -> reverse' xs (x::acc)
  
let reverse l =
  reverse' l []

let rec fold f acc l =
  match l with
  | [] -> acc
  | x::xs -> fold f (f acc x) xs

let even' x =
  x mod 2 = 0

let even_fold f_filter acc x =
  match (f_filter x) with
  | true -> x::acc
  | false -> acc

let even acc x =
  even_fold even' acc x

let filter_with_fold' f l acc =
  reverse (fold f acc l)

let filter_with_fold f l =
  filter_with_fold' f l []
  
let () =
  [1; 2; 5; 7; 34]
  |> filter_with_fold even
  |> List.iter (Printf.printf "%i "); *)