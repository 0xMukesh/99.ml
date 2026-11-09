let () =
  let open Problems in
  let result = list_replicate_elements [ "a"; "b"; "c" ] 3 in
  List.iter (fun s -> print_endline s) result
;;
