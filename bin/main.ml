open Core

module Simply_typed_repl = struct
  (** Enables multiline inputs by waiting for [\n\n] instead *)
  let multiline = ref false

  (** Gets line for stdin, waits for double newlines if [multiline] is on *)
  let rec get_input () =
    Out_channel.flush stdout;
    let line = In_channel.(input_line stdin) in
    match line with
    | None -> exit 0
    | Some line ->
      if !multiline && not (String.is_empty line || Char.equal (String.get line 0) ':')
      then line ^ "\n" ^ get_input ()
      else line
  ;;

  let rec repl () =
    print_string ">>> ";
    let input = get_input () in
    (match input with
     | c when List.mem [ ":q"; ":quit" ] c ~equal:String.equal ->
       print_endline "Goodbye!";
       exit 0
     | c when List.mem [ ":h"; ":help" ] c ~equal:String.equal ->
       print_endline
       @@ ":h(elp) -> View this message\n"
       ^ ":q(uit) -> Exit REPL\n"
       ^ ":m(ulti) -> Toggle multiline mode, uses double newlines to run"
     | c when List.mem [ ":m"; ":multi" ] c ~equal:String.equal ->
       multiline := not !multiline;
       print_endline @@ "Set multiline mode to " ^ string_of_bool !multiline
     | "" -> ()
     | _ ->
       (try Simply_typed_extended.repl input with
        | e -> print_endline (Exn.to_string e)));
    repl ()
  ;;

  let start () =
    print_endline "Welcome to the REPL (type ':h(elp)' for meta commands)";
    repl ()
  ;;
end

let command =
  Command.basic
    ~summary:"Simple REPL interface"
    (let open Command.Let_syntax in
     let%map_open impl =
       flag_optional_with_default_doc
         "-impl"
         string
         Sexp.of_string
         ~default:"simply_typed_extended"
         ~doc:"STRING implementation to run"
     in
     fun () ->
       match impl with
       | "simply_typed_extended" -> Simply_typed_repl.start ()
       | _ -> print_endline "unknown implementation")
;;

let () = Command_unix.run command
