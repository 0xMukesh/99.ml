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

type 'a node =
  | One of 'a
  | Many of 'a node list

let list_flatten (l : 'a node list) =
  let rec loop l acc =
    match l with
    | [] -> acc
    | One x :: tl -> loop tl (x :: acc)
    | Many x :: tl -> loop tl (loop x acc)
  in
  list_rev @@ loop l []
;;

let list_eliminate_duplicates l =
  let rec loop l acc =
    match l with
    | [] -> acc
    | [ a ] -> a :: acc
    | a :: (b :: _ as tl) -> if a = b then loop tl acc else loop tl (a :: acc)
  in
  list_rev @@ loop l []
;;

let list_pack_consecutive_elements l =
  let rec loop l curr acc =
    match l with
    | [] -> acc
    | [ a ] -> (a :: curr) :: acc
    | a :: (b :: _ as tl) ->
      if a = b then loop tl (a :: curr) acc else loop tl [] ((a :: curr) :: acc)
  in
  list_rev @@ loop l [] []
;;

type 'a rle =
  | One of 'a
  | Many of int * 'a

let list_rle_encode l =
  let encode cnt elem = if cnt = 1 then One elem else Many (cnt, elem) in
  let rec loop l curr acc =
    match l with
    | [] -> acc
    | [ a ] -> encode (curr + 1) a :: acc
    | a :: (b :: _ as tl) ->
      if a = b then loop tl (curr + 1) acc else loop tl 0 (encode (curr + 1) a :: acc)
  in
  list_rev @@ loop l 0 []
;;

let rec duplicate elem cnt acc =
  if cnt <= 0 then acc else duplicate elem (cnt - 1) (elem :: acc)
;;

let list_rle_decode l =
  let rec loop l acc =
    match l with
    | [] -> acc
    | One x :: tl -> loop tl (x :: acc)
    | Many (cnt, x) :: tl -> loop tl (duplicate x cnt acc)
  in
  list_rev @@ loop l []
;;

let list_replicate_elements l n =
  let rec loop l acc =
    match l with
    | [] -> acc
    | hd :: tl -> loop tl (duplicate hd n acc)
  in
  list_rev @@ loop l []
;;
