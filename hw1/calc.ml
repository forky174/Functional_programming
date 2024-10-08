(* 
type expr =
  | Num of int
  | Op of expr * (int -> int -> int) * expr

let example =
  Op (Num 2, ( + ), Op (Num 3, ( * ), Num 4))

let rec eval e =
  match e with
  | Num n -> n
  | Op (e1, operation , e2) -> operation (eval e1) (eval e2) *)

type op = Plus | Minus | Times | Slash

type expr =
  | Num of int
  | Op of expr * op * expr

let example =
  Op (Op (Num 100, Slash, Num 5), Minus, Op (Num 2, Plus, Op (Num 3, Times, Num 4)))

let rec eval e =
  match e with
  | Num n -> n
  | Op (e1, Plus , e2)  -> ((eval e1) + (eval e2))
  | Op (e1, Minus , e2) -> ((eval e1) - (eval e2))
  | Op (e1, Times , e2) -> ((eval e1) * (eval e2))
  | Op (e1, Slash , e2) -> ((eval e1) / (eval e2))

let () =
  example
  |> eval
  |> Printf.printf "%i\n";

(* 
let error message s start i =
  Printf.eprintf "Error: %s\n" message;
  Printf.eprintf " %s\n" s;
  Printf.eprintf " %s%s\n"
    (String.make start ' ') (String.make (i - start) '^');
  raise Exit

let rec parse_number s start i =
  match s.[i] with
  | '0'..'9' ->
    parse_number s start (i + 1)
  | _ | exception Invalid_argument _ ->
    let token = String.sub s start (i - start) in
    match int_of_string_opt token with
    | Some n -> (Num n, i)
    | None -> error "invalid numeral" s start i

let rec skip_whitespace s i =
  match s.[i] with
  | ' ' | '\t' | '\n' | '\r' -> skip_whitespace s (i + 1)
  | _ | exception Invalid_argument _ -> i

let rec parse_parenthesis s i =
  let start = i in
  let i = skip_whitespace s (i + 1) in
  let (expr, i) = parse_additive s i in
  let i = skip_whitespace s i in
  match s.[i] with
  | ')' -> (expr, i + 1)
  | _ | exception Invalid_argument _ ->
    error "unclosed parenthesis" s start (start + 1)

and parse_multiplicative s i =
  let parse_operand s i =
    match s.[i] with
    | '0'..'9' | '-' -> parse_number s i (i + 1)
    | '(' -> parse_parenthesis s i
    | _ | exception Invalid_argument _ ->
      error "expected an expression" s i (i + 1)
  in

  let (initial, i) = parse_operand s i in

  let rec parse_more s left i =
    let i = skip_whitespace s i in
    match s.[i] with
    | '*' | '/' as operator ->
      let i = skip_whitespace s (i + 1) in
      let (right, i) = parse_operand s i in
      let operator = if operator = '*' then ( * ) else ( / ) in
      parse_more s (Op (left, operator, right)) i
    | _ | exception Invalid_argument _ ->
      (left, i)
  in
  parse_more s initial i

and parse_additive s i =
  let parse_operand s i =
    match s.[i] with
    | '0'..'9' | '.' | '-' | '(' -> parse_multiplicative s i
    | _ | exception Invalid_argument _ ->
      error "expected an expression" s i (i + 1)
  in

  let (initial, i) = parse_operand s i in

  let rec parse_more s left i =
    let i = skip_whitespace s i in
    match s.[i] with
    | '+' | '-' as operator ->
      let i = skip_whitespace s (i + 1) in
      let (right, i) = parse_operand s i in
      let operator = if operator = '+' then ( + ) else ( - ) in
      parse_more s (Op (left, operator, right)) i
    | _ | exception Invalid_argument _ ->
      (left, i)
  in
  parse_more s initial i

let parse s =
  let i = skip_whitespace s 0 in
  let (expr, i) = parse_additive s i in
  if i < String.length s then
    error "unexpected trailing symbols" s i (String.length s);
  expr *)