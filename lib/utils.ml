open Core

let counter = ref 0

(* TODO: [gen_sym] should be a module functor so that tests are deterministic across modules *)
let gen_sym () =
  let symbol = "x." ^ Int.to_string !counter in
  counter := !counter + 1;
  symbol
;;
