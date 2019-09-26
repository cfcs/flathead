open Type

type t =
{
  interpreter : Interpreter.t;
}

let make interpreter = {
  interpreter = interpreter;
}

let clear_screen () =
  print_string "\027[2J"

let cursor_home () =
  print_string "\027[H"

let draw_screen screen =
  cursor_home ();
  (* draw_status screen; *)
  let rec aux n =
    let (Character_height h) = Screen.height screen in
    if n < h then (
      let text = Screen.get_line_at screen (Character_y (n + 1)) in
      print_string text;
      aux (n + 1)) in
  aux 0 ;
  flush stdout

let needs_more runner =
  let screen = Interpreter.screen runner.interpreter in
  Screen.needs_more screen

let draw_interpreter runner =
  let interpreter = runner.interpreter in
  let screen = Interpreter.screen interpreter in
  let state = Interpreter.state interpreter in
  let input = Interpreter.input interpreter in
  let has_new_output = Interpreter.has_new_output interpreter in
  if state = Interpreter.Waiting_for_input then
    draw_screen (Screen.fully_scroll (Screen.print screen input))
  else if has_new_output then
    let screen_to_draw =
      if needs_more runner then
        Screen.more screen
      else
        screen in
    draw_screen screen_to_draw

let get_char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false; Unix.c_echo = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

let do_step runner =
  let interpreter = runner.interpreter in
  let state = Interpreter.state interpreter in
  let new_interpreter = match state with
  | Interpreter.Waiting_for_input ->
      Interpreter.step_with_input interpreter (get_char ())
  | Interpreter.Halted ->
      failwith "Halted"
  | Interpreter.Running ->
      Interpreter.step interpreter
  in
  { interpreter = new_interpreter }

let rec main_loop runner =
  let r = do_step runner in
  draw_interpreter r;
  main_loop r

let run runner =
  clear_screen ();
  main_loop runner
