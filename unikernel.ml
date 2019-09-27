open Lwt.Infix
open Type

module Main
    (KV: Mirage_kv_lwt.RO)
    (S: Mirage_stack_lwt.V4) = struct

  let log_new flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "[%a:%d] New connection"
                  Ipaddr.V4.pp dst dst_port)

  let log_read_err flow e =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.warn (fun f -> f "[%a:%d] Read error: %a, closing connection"
                  Ipaddr.V4.pp dst dst_port S.TCPV4.pp_error e)

  let log_write_err flow e =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.warn (fun f -> f "[%a:%d] Write error: %a, closing connection"
                  Ipaddr.V4.pp dst dst_port S.TCPV4.pp_write_error e)

  let log_closing flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "[%a:%d] Closing connection"
                  Ipaddr.V4.pp dst dst_port)

  let write_string flow s =
    (* Logs.info (fun f -> f "Output: %S" s); *)
    S.TCPV4.write flow (Server.encode (Cstruct.of_string s))

  let draw_screen flow screen =
    let rec aux n =
      let (Character_height h) = Screen.height screen in
      if n < h then (
        let text = Screen.get_line_at screen (Character_y (n + 1)) in
        write_string flow text >>= function
          | Error e -> (* TODO: We should terminate execution here *)
              log_write_err flow e; S.TCPV4.close flow
          | Ok () ->
              aux (n + 1))
      else Lwt.return_unit
    in
    aux 0

  let draw_interpreter flow interpreter : unit Lwt.t =
    let screen = Interpreter.screen interpreter in
    let state = Interpreter.state interpreter in
    let input = Interpreter.input interpreter in
    let has_new_output = Interpreter.has_new_output interpreter in
    if state = Interpreter.Waiting_for_input then
      draw_screen flow (Screen.fully_scroll (Screen.print screen input))
    else if has_new_output then
      let screen_to_draw =
        if Screen.needs_more screen then
          Screen.more screen
        else
          screen in
      draw_screen flow screen_to_draw
    else
      Lwt.return_unit

  let start kv s : unit Lwt.t =

    (* Run a single Interpreter step *)
    let rec session_step flow telnet interpreter input : unit Lwt.t =

      let rec read_more_input telnet =
        S.TCPV4.read flow >>= function
          | Ok `Eof ->
              Lwt.return None
          | Error e ->
              log_read_err flow e; Lwt.return None
          | Ok (`Data b) ->
              let telnet, events, out = Server.handle telnet b in
              S.TCPV4.write flow out >>= function
                | Error e ->
                    log_write_err flow e; Lwt.return None
                | Ok () ->
                    let read_characters = List.fold_left (fun acc event ->
                      match event with
                      | `Resize (_w, _h) ->
                          acc (* TODO *)
                      | `Data bytes ->
                          let s = Cstruct.to_string bytes in
                          let chars = Array.make (String.length s) ' ' in
                          String.iteri (fun i c -> chars.(i) <- c) s;
                          Array.to_list chars @ acc) [] events
                    in
                    match read_characters with
                    | [] ->
                        read_more_input telnet
                    | _ ->
                        Lwt.return (Some (telnet, read_characters))
      in

      let run = match Interpreter.state interpreter with
      | Interpreter.Running ->
          Lwt.return (Some (telnet, Interpreter.step interpreter, input))
      | Interpreter.Waiting_for_input ->
          begin
            match input with
            | [] ->
              read_more_input telnet >>= begin function
                | None -> Lwt.return None
                | Some (telnet, input) ->
                    (* Logs.info (fun f -> f
                      "Input: %a" Fmt.(list ~sep:(unit ",") char) input); *)
                    Lwt.return (Some (telnet, interpreter, input))
              end
            | ch :: tl ->
              Lwt.return (Some
                (telnet, Interpreter.step_with_input interpreter ch, tl))
          end
      | Interpreter.Halted ->
          Lwt.return None
      in

      run >>= function
        | None ->
            log_closing flow; S.TCPV4.close flow
        | Some (new_telnet, new_interpreter, input) ->
            draw_interpreter flow new_interpreter >>= fun () ->
            session_step flow new_telnet new_interpreter input

    (* Handle a new connection; setup the Interpreter *)
    and session_start kv flow telnet =
      log_new flow;
      let get_story_content filename =
        KV.get kv (Mirage_kv.Key.v filename) >|= function
        | Error _e ->
            failwith "could not get story from KV"
        | Ok file ->
            file
      in
      get_story_content "MiniZork.Z3" >>= fun story_content ->
      let story = Story.load story_content in
      let screen = Screen.make (Character_height 50) (Character_width 80) in
      let interpreter = Interpreter.make story screen in
      session_step flow telnet interpreter []

    in
    let port = Key_gen.port () in
    Logs.info (fun m -> m "Listening on [%a:%d]"
                  Fmt.(list Ipaddr.V4.pp) (S.(IPV4.get_ip @@ ipv4 s)) port);
    S.listen_tcpv4 s ~port (fun flow ->
      let telnet, out = Server.init () in
      S.TCPV4.write flow out >>= function
        | Error e ->
            log_write_err flow e; S.TCPV4.close flow
        | Ok () ->
            session_start kv flow telnet
    );

    S.listen s
end
