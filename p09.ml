open OUnit2

(* Pack consecutive duplicates of list elements into sublists. *)

let pack l =
  let rec aux l curr temp output =
    match l with
    | [] -> output @ [temp]
    | h :: t ->
      match curr with
      | None -> aux t (Some h) [h] output
      | Some a when h <> a -> aux t (Some h) [h] (output @ [temp])
      | _ -> aux t curr (temp @ [h]) output in
  aux l None [] []

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]] (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"])
]

let () =
  run_test_tt_main ("tests" >::: tests)
