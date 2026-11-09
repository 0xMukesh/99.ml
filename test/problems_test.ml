open Alcotest

let test_list_tail () =
  check
    (option string)
    "last element"
    (Some "d")
    (Problems.list_tail [ "a"; "b"; "c"; "d" ]);
  check (option string) "empty list" None (Problems.list_tail [])
;;

let test_list_last_two_elems () =
  check
    (option (pair string string))
    "last two elements"
    (Some ("c", "d"))
    (Problems.list_last_two_elems [ "a"; "b"; "c"; "d" ]);
  check
    (option (pair string string))
    "one element list"
    None
    (Problems.list_last_two_elems [ "a" ])
;;

let test_list_nth_elem () =
  let input = [ "a"; "b"; "c"; "d"; "e" ] in
  check (option string) "first element" (Some "a") (Problems.list_nth_elem input 0);
  check (option string) "out of bounds" None (Problems.list_nth_elem input 6);
  check (option string) "negative index" None (Problems.list_nth_elem input (-1))
;;

let test_list_len () =
  check int "list length" 3 (Problems.list_len [ "a"; "b"; "c" ]);
  check int "empty list" 0 (Problems.list_len [])
;;

let test_list_rev () =
  check
    (list string)
    "reverses list"
    [ "c"; "b"; "a" ]
    (Problems.list_rev [ "a"; "b"; "c" ]);
  check (list string) "empty list" [] (Problems.list_rev [])
;;

let test_list_flatten () =
  check
    (list string)
    "flatten list"
    [ "a"; "b"; "c"; "d"; "e" ]
    (Problems.list_flatten
       [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
    )
;;

let test_list_eliminate_duplicates () =
  check
    (list string)
    "eliminate duplicates"
    [ "a"; "b"; "c"; "a"; "d"; "e" ]
    (Problems.list_eliminate_duplicates
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    )
;;

let test_list_pack_consecutive_elements () =
  check
    (list (list string))
    "pack consecutive elems"
    [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
    (Problems.list_pack_consecutive_elements
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
    )
;;

let rle_testable =
  testable
    (fun ppf (value : string Problems.rle) ->
       match value with
       | One x -> Fmt.pf ppf "One %s" x
       | Many (cnt, x) -> Fmt.pf ppf "Many (%d, %s)" cnt x
     )
    ( = )
;;

let test_list_rle_encode () =
  check
    (list rle_testable)
    "rle encode"
    [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
    (Problems.list_rle_encode
       [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    )
;;

let test_list_rle_decode () =
  check
    (list string)
    "rle decode"
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    (Problems.list_rle_decode
       [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
    )
;;

let test_list_replicate_elems () =
  check
    (list string)
    "replicate elems"
    [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
    (Problems.list_replicate_elements [ "a"; "b"; "c" ] 3)
;;

let () =
  let test_cases =
    [ ( "lists"
      , [ ("tail", test_list_tail)
        ; ("last two elements", test_list_last_two_elems)
        ; ("nth element", test_list_nth_elem)
        ; ("length", test_list_len)
        ; ("reverse", test_list_rev)
        ; ("flatten", test_list_flatten)
        ; ("eliminate duplicates", test_list_eliminate_duplicates)
        ; ("pack consecutive elements", test_list_pack_consecutive_elements)
        ; ("rle encode", test_list_rle_encode)
        ; ("rle decode", test_list_rle_decode)
        ; ("replicate elements", test_list_replicate_elems)
        ]
      )
    ]
  in
  run
    "99 ocaml problems"
    (List.map
       (fun (cat, cases) ->
          (cat, List.map (fun (name, f) -> test_case name `Quick f) cases)
        )
       test_cases
    )
;;
