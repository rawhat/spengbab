open Core

let usage_msg = "spengbab [<string>]"

let words : (string list) ref = ref []

let input_words word =
  words := word :: !words

let spengbab_word (word : string) =
  String.map word ~f:(fun char ->
    match Random.int(2) with
      | 0 -> Char.lowercase char
      | 1 -> Char.uppercase char
      | _ -> char
  )

let () =
  Arg.parse [] input_words usage_msg;
  let
    results =
      !words
      |> List.rev
      |> List.map ~f:spengbab_word
      |> String.concat ~sep:" "
  in
    print_endline results
