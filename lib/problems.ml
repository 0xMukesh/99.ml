let rec list_tail l =
  match l with
  | [] -> None
  | [ a ] -> Some a
  | _ :: tl -> list_tail tl
;;

let rec list_last_two_elems l =
  match l with
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | a :: b :: tl -> list_last_two_elems tl
;;

let rec list_nth_elem n l =
  match l with
  | [] -> None
  | hd :: tl -> if n = 0 then Some hd else list_nth_elem (n - 1) tl
;;

let rec list_len l =
  let rec loop l acc =
    match l with
    | [] -> acc
    | _ :: tl -> loop tl (acc + 1)
  in
  loop l 0
;;

let rec list_rev l =
  let rec loop l acc =
    match l with
    | [] -> acc
    | hd :: tl -> loop tl (hd :: acc)
  in
  loop l []
;;

let list_is_palindrome l = l = list_rev l
