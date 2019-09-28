open Lwt.Infix
open Type

module Main
    (KV: Mirage_kv_lwt.RO)
    (S: Mirage_stack_lwt.V4) = struct

  let log_closing flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "[%a:%d] Closing connection"
                  Ipaddr.V4.pp dst dst_port)

  let log_read_err flow e =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.warn (fun f -> f "[%a:%d] Read error: %a, closing connection"
                  Ipaddr.V4.pp dst dst_port S.TCPV4.pp_error e)

  let log_write_err _flow e =
    ()
    (*
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.warn (fun f -> f "[%a:%d] Write error: %a, closing connection"
                  Ipaddr.V4.pp dst dst_port S.TCPV4.pp_write_error e)*)

  module Player : sig
    type connection
    val attach_single : S.TCPV4.flow -> Server.state -> connection
    val attach_multi : S.TCPV4.flow -> Server.state -> connection
    val detach : S.TCPV4.flow -> unit Lwt.t
    val is_connected : unit -> bool
    val broadcast : Cstruct.t ->
      (unit, S.TCPV4.write_error) result S.TCPV4.io
    val write_string : connection -> string ->
      (unit, unit) result S.TCPV4.io
    val stop_playing : connection -> unit Lwt.t
    val read_input : connection -> char list option Lwt.t
  end = struct
    module M = Map.Make(struct
      type t = S.TCPV4.flow
      let compare = compare
    end)

    type t = {
      telnet: Server.state
    }

    type connection =
    | Single of S.TCPV4.flow * Server.state * char list Lwt_mvar.t
    | Multi

    (* Global mutable state *)
    let all : t M.t ref = ref M.empty
    let global_input_queue : char list Lwt_mvar.t = Lwt_mvar.create_empty ()

    let read_input conn =
      let queue = match conn with
      | Single (_, _, mvar) -> mvar
      | Multi -> global_input_queue
      in
      (* if we are not in multiplayer mode this is where we return None
         to signal client has left the building. todo *)
      Lwt_mvar.take queue >|= fun lst -> Some lst

    let unicast flow cs =
      S.TCPV4.write flow cs >>= function
      | Ok _ -> Lwt.return (Ok ())
      | Error e -> all := M.remove flow !all ;
        log_write_err flow e ;
        S.TCPV4.close flow >|= fun () -> Error e

    let broadcast cs =
      Logs.warn (fun m -> m"broadcasting...");
      M.fold (fun flow _value effect ->
          effect >>= fun () ->
          Logs.warn (fun m -> m "broadcasting %a" Cstruct.hexdump_pp cs);
          S.TCPV4.write flow cs >>= function
          | Ok _ -> Lwt.return_unit
          | Error e -> all := M.remove flow !all ;
            log_write_err flow e ;
            S.TCPV4.close flow
        ) !all Lwt.return_unit
      >|= fun () -> Ok ()

    let is_connected () = M.is_empty !all

    let detach flow =
      all := M.remove flow !all ;
      Logs.warn (fun m -> m"detaching.");
      log_closing flow ;
      S.TCPV4.close flow

    let attach_input_listener flow telnet input_mvar () =
      let rec loop input_prefix =
        S.TCPV4.read flow >>= function
        | Ok `Eof -> Lwt.catch (fun () -> detach flow) (fun _ -> Lwt.return())
        | Error _ -> Lwt.catch (fun () -> detach flow) (fun _ -> Lwt.return())
        | Ok (`Data b) ->
          let telnet, events, out = Server.handle telnet b in
          unicast flow out >>= fun _ -> (* todo local echo *)
          List.fold_left (fun input_prefix event ->
              input_prefix >>= fun input_prefix ->
              match event with
              | `Resize (_w, _h) ->
               Lwt.return input_prefix (* TODO *)
              | `Data bytes ->
                let next = input_prefix ^ Cstruct.to_string b in
                Logs.warn (fun m -> m "got data %S" next);
                begin match String.split_on_char '\n' next with
                  | [] | [ _ ] -> Lwt.return next
                  | completed::tl ->
                    let completed = String.trim completed ^ "\r" in
                    Lwt_mvar.put input_mvar
                      (Array.init (String.length completed)
                         (String.get completed) |> Array.to_list)
                    >|= fun () -> (String.concat "\n" tl)
                end
            ) (Lwt.return input_prefix) events
          >>= fun input_prefix -> loop input_prefix
      in loop ""

    let attach_single flow telnet =
      Logs.info (fun m -> m "new single player client.") ;
      let input_mvar = Lwt_mvar.create_empty () in
      Lwt.async (attach_input_listener flow telnet input_mvar);
      Single (flow, telnet, input_mvar)

    let attach_multi flow telnet =
      all := M.add flow {telnet} !all ;
      Logs.info (fun m -> m "new client. %d multiplayers connected."
                    (M.cardinal !all)) ;
      Lwt.async (attach_input_listener flow telnet global_input_queue) ;
      Multi

    let write_string conn s =
      (* Logs.info (fun f -> f "Output: %S" s); *)
      let cs =
        (* well technically speaking the server should probably require
           a telnet client state in order to know how to encode this, but
           oh well. *)
        Server.encode (Cstruct.of_string s) in
      match conn with
      | Multi _ -> broadcast cs
      | Single (flow, _, _) ->
        S.TCPV4.write flow cs >>= function
        | Ok () -> Lwt.return (Ok ())
        | Error _ ->
          Lwt.fail_with "writing to player failed."

    let stop_playing conn = match conn with
      | Single (flow, _ , _) ->
        Logs.warn (fun m -> m "%s" __LOC__);
        detach flow
      | Multi _ ->
        M.fold (fun flow _val ef ->
            ef >>= fun () ->
            Logs.warn (fun m -> m "%s" __LOC__);
            detach flow) !all Lwt.return_unit

  end
  include Player

  let multiplayer = ref false

  let log_new flow =
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "[%a:%d] New connection"
                  Ipaddr.V4.pp dst dst_port)

  let draw_screen flow screen =
    let rec aux n =
      let (Character_height h) = Screen.height screen in
      if n < h then (
        let text = Screen.get_line_at screen (Character_y (n + 1)) in
        write_string flow text >>= function
          | Error () -> (* TODO: We should terminate execution here *)
            failwith "todo writing"
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
    let rec session_step (flow:connection) interpreter input : unit Lwt.t =
      let run = match Interpreter.state interpreter with
      | Interpreter.Running ->
          Lwt.return (Some (Interpreter.step interpreter, input))
      | Interpreter.Waiting_for_input ->
          begin
            match input with
            | [] ->
              Player.read_input flow >>= begin function
                | None -> Lwt.return None
                | Some input ->
                    (* Logs.info (fun f -> f
                      "Input: %a" Fmt.(list ~sep:(unit ",") char) input); *)
                    Lwt.return (Some (interpreter, input))
              end
            | ch :: tl ->
              Lwt.return (Some
                (Interpreter.step_with_input interpreter ch, tl))
          end
      | Interpreter.Halted ->
          Lwt.return None
      in

      run >>= function
      | None ->
        stop_playing flow
      | Some (new_interpreter, input) ->
        draw_interpreter flow new_interpreter >>= fun () ->
        session_step flow new_interpreter input

    (* Handle a new connection; setup the Interpreter *)
    and session_start kv conn telnet =
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
      session_step conn interpreter []
    and multiplayer_attach flow telnet =
      log_new flow;
      Player.attach_multi flow telnet |> ignore ;
      Lwt.return_unit
    in
    let port = Key_gen.port () in
    if Key_gen.multiplayer () then
        multiplayer := true ;
    Logs.info (fun m -> m "Listening on [%a:%d]"
                  Fmt.(list Ipaddr.V4.pp) (S.(IPV4.get_ip @@ ipv4 s)) port);
    S.listen_tcpv4 s ~port (fun flow ->
        let telnet, out = Server.init () in
        log_new flow ;
      S.TCPV4.write flow out >>= function
        | Error e ->
            log_write_err flow e; S.TCPV4.close flow
        | Ok () ->
          if !multiplayer
          then multiplayer_attach flow telnet
          else session_start kv (Player.attach_single flow telnet) telnet
    );

    S.listen s
end
