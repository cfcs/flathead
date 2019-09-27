open Lwt.Infix

module Main
    (KV: Mirage_kv_lwt.RO)
    (S: Mirage_stack_lwt.V4) = struct

  let log_new flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "[%a:%d] New connection"
                  Ipaddr.V4.pp dst dst_port)

  let log_write_err flow e =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.warn (fun f -> f "[%a:%d] Write error: %a, closing connection"
                  Ipaddr.V4.pp dst dst_port S.TCPV4.pp_write_error e)

  let log_closing flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "[%a:%d] Closing connection"
                  Ipaddr.V4.pp dst dst_port)

  let start kv s : unit Lwt.t =

    let rec session_step flow interpreter =
      let new_interpreter = match Interpreter.state interpreter with
      | Interpreter.Waiting_for_input ->
          S.TCPV4.close flow >|= fun () -> None
      | Interpreter.Halted ->
          S.TCPV4.close flow >|= fun () -> None
      | Interpreter.Running ->
          Lwt.return (Some (Interpreter.step interpreter))
      in
      new_interpreter >>= function
      | None -> Lwt.return_unit
      | Some new_interpreter -> session_step flow new_interpreter

    and session_start kv flow =
      log_new flow;
      let get_story_content filename =
        KV.get kv (Mirage_kv.Key.v filename) >|= function
        | Error e -> failwith "error"
        | Ok file -> file
      in
      get_story_content "MiniZork.Z3" >>= fun story_content ->
      let story = Story.load story_content in
      let screen = Screen.make (Character_height 50) (Character_width 80) in
      let interpreter = Interpreter.make story screen in
      session_step flow interpreter

    in
    let port = Key_gen.port () in
    Logs.info (fun m -> m "Listening on [%a:%d]"
                  Fmt.(list Ipaddr.V4.pp) (S.(IPV4.get_ip @@ ipv4 s)) port);
    S.listen_tcpv4 s ~port (fun flow ->
      session_start kv flow
    );

    S.listen s
end
