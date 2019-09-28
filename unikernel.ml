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

  let log_write_err _flow _e =
    ()
    (*
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.warn (fun f -> f "[%a:%d] Write error: %a, closing connection"
                  Ipaddr.V4.pp dst dst_port S.TCPV4.pp_write_error e)*)


  module Screen_queue = struct
    module M = Map.Make(struct
        type t = int
        let compare = compare
      end)

    type t = {
      cond : unit Lwt_condition.t ;
      mutable queue: string M.t ;
    }

    let create () =
      let queue = M.empty |> M.add 0 "Welcome to multiplayer Z-Machine" in
      let cond = Lwt_condition.create () in
      { queue; cond ; }

    let scrollback_len = 80

    let subscribe t f =
      let rec loop (seen:M.key) =
        let seen, to_send =
          fst (M.max_binding t.queue),
          M.to_seq_from seen t.queue
          |> Seq.filter_map (fun (idx,to_send) ->
              if idx > seen then (
                Logs.warn (fun m -> m "should send idx %d:%S" idx to_send);
                Some to_send) else
                (Logs.warn (fun m -> m "skip");None)) in

        (* Spawn async thread to not have to resolve
           Lwt promises before calling {!wait} again.
           This should make {loop} atomic, ensuring we do not
           miss out on signals. *)
        Logs.warn (fun m -> m "subscribe: spawning async");
        Lwt.async (f to_send);
        Lwt_condition.wait t.cond >>= fun () ->
        Logs.warn (fun m -> m "COND.");
        loop seen
      in loop (fst (M.min_binding t.queue))

    let append_lines t (string_lst:string list) =
      (*Logs.warn (fun m -> m "append-lines %s" __LOC__);*)
      let current_top = try fst (M.max_binding t.queue) with Not_found -> 0 in
      let (next_top:int), (lst:(M.key * string) list) =
        List.fold_left (fun (idx,acc) s ->
            let idx : int = succ idx in
            idx, ((idx, s)::acc))
          (current_top, []) string_lst
      in
      let this_batch = next_top - current_top in
      (* add to queue (it can temporarily be larger than [scrollback_len]: *)
      t.queue <- M.add_seq (List.to_seq lst) t.queue ;
      begin if fst (M.max_binding t.queue)
               - fst (M.min_binding t.queue)
               > scrollback_len then
          (* drop early things (but ensure there is at least [this_batch]
             elements: *)
          let threshold =
            min (current_top - this_batch) (current_top - scrollback_len) in
          t.queue <- M.filter (fun key _v -> key > threshold) t.queue
      end ;
      Lwt_condition.broadcast t.cond ()

end

  module Player : sig
    type t
    type connection
    val attach_single : S.TCPV4.flow -> Server.state -> connection
    val attach_multi : S.TCPV4.flow -> Server.state -> unit Lwt.t
    val is_connected : unit -> bool
    val broadcast : string -> unit Lwt.t
    val write_string : connection -> string -> unit Lwt.t
    val stop_playing : connection -> unit Lwt.t
    val read_input : connection -> char list option Lwt.t
    val multi_connection : connection
  end = struct

    module M = Map.Make(struct
      type t = S.TCPV4.flow
      let compare = compare
    end)

    type t = { flow: S.TCPV4.flow;
               input : char list Lwt_mvar.t ;
               output : Screen_queue.t ;
             }

    type connection =
      | Single of t
      | Multi

    let multi_connection = Multi

    (* Global mutable state *)
    let all : t M.t ref = ref M.empty
    let global_input_queue : char list Lwt_mvar.t = Lwt_mvar.create_empty ()
    let global_screen : Screen_queue.t = Screen_queue.create ()

    let read_input conn =
      let queue = match conn with
      | Single { input ; _ } -> input
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
      Logs.warn (fun m -> m "broadcasting %s" cs);
      Screen_queue.append_lines global_screen [cs] ;
      Lwt.return_unit

    let is_connected () = not (M.is_empty !all)

    let detach flow =
      all := M.remove flow !all ;
      Logs.warn (fun m -> m"detaching.");
      log_closing flow ;
      (* todo what the fuck
         Fatal error: exception Unix.Unix_error(Unix.EBADF, "check_descriptor", "")
         Raised at file "src/unix/lwt_unix.cppo.ml", line 353, characters 4-64
         Called from file "src/unix/lwt_unix.cppo.ml", line 1639, characters 2-21
         Called from file "src/stack-unix/tcpv4_socket.ml", line 40, characters 8-31
         Called from file "unikernel.ml", line 9, characters 24-40
         Called from file "unikernel.ml", line 152, characters 6-22
         Called from file "unikernel.ml", line 212, characters 15-469
         Called from file "src/core/lwt.ml", line 2463, characters 16-20
      *)
      try S.TCPV4.close flow with _ -> Lwt.return_unit

    let attach_input_listener flow orig_telnet input_mvar () =
      Logs.warn (fun m -> m "input listener attached.");
      let rec loop telnet input_prefix =
        Logs.warn (fun m -> m "input listener looping.");
        S.TCPV4.read flow >>= function
        | Ok `Eof -> Lwt.catch (fun () -> detach flow) (fun _ -> Lwt.return())
        | Error _ -> Lwt.catch (fun () -> detach flow) (fun _ -> Lwt.return())
        | Ok (`Data b) ->
          Logs.warn (fun m -> m "input listener got data");
          let telnet, events, out = Server.handle telnet b in
          Logs.warn (fun m -> m "handled telnet");
          unicast flow out >>= fun _ -> (* todo local echo *)
          List.fold_left (fun input_prefix event ->
              input_prefix >>= fun input_prefix ->
              match event with
              | `Resize (_w, _h) ->
               Lwt.return input_prefix (* TODO *)
              | `Data bytes ->
                let completed, next =
                  String.to_seq (input_prefix ^ Cstruct.to_string bytes)
                  |> Seq.fold_left (fun (has_cmd, cmd, was_newline,acc) ch ->
                      match ch , has_cmd, was_newline with
                      | '\000', _ , _ ->
                        (* some telnet modes emit CR NUL for linebreaks*)
                        has_cmd, cmd, false, acc
                      | ('\r'|'\n'), false, false ->
                        (* found newline ending cmd, not adding to acc*)
                        true, cmd, true, acc
                      | ('\r'|'\n'), true, false ->
                        (* already have cmd; found newline in acc *)
                        true, cmd, true, '\r'::acc

                      | ('\r'|'\n'), true, true -> (* collapsing newlines *)
                        has_cmd, cmd, true, acc
                      | ch, false, true ->
                        true, cmd, false, ch::acc
                      | ch, true, _ ->
                        (* adding to tl *)
                        true, cmd, false, ch::acc
                      | ch, false, false ->
                        (*addding to cmd *)
                        false, ch::cmd, false, acc
                    ) (false, [], false,[])
                  |> function (_, cmd, _, next) ->
                    cmd,
                    next |> List.rev |> List.to_seq |> String.of_seq in
                let completed = (List.rev completed |> List.to_seq
                                 |> String.of_seq |> String.trim)
                                ^ "\r" (* expected by z-machine*) in
                Lwt_mvar.put input_mvar
                  (Array.init (String.length completed)
                     (String.get completed) |> Array.to_list)
                >|= fun () -> next
            ) (Lwt.return input_prefix) events
          >>= fun input_prefix -> loop telnet input_prefix
      in loop orig_telnet ""

    let write_string conn s =
      (* Logs.info (fun f -> f "Output: %S" s); *)
      match conn with
      | Multi -> broadcast s
      | Single { output ; _  } ->
        Screen_queue.append_lines output [s] ;
        Lwt.return_unit

    let attach_output_thread flow output () =
      Screen_queue.subscribe output
        (fun seq () ->
           Logs.warn (fun m -> m "subscribing");
           let rec loop = function
             | Seq.Nil ->
               Logs.warn (fun m -> m "done reading queue.");
               Lwt.return_unit
             | Seq.Cons (s, next) ->
               Logs.warn (fun m -> m "about to send %S" s);
               let cs =
                 (* well technically speaking the server should probably require
                    a telnet client state in order to know how to encode this, but
                    oh well. *)
                 Server.encode (Cstruct.of_string s) in
               Lwt.catch
                 (fun () -> S.TCPV4.write flow cs >>= begin function
                      | Ok () -> Lwt.return ()
                      | Error _ -> (* todo *)
                        Logs.warn (fun m -> m "Player disconnected.");
                        detach flow
                    end)
                 (fun _ ->
                    Logs.warn (fun m -> m "Player disconnected. Mirage threw exception.");
                    detach flow)
               >>= fun () -> loop (next ())
           in
           loop @@ seq()
        )

    let attach_single flow telnet =
      Logs.info (fun m -> m "new single player client.") ;
      let input_mvar = Lwt_mvar.create_empty () in
      Lwt.async (attach_input_listener flow telnet input_mvar);
      let output = Screen_queue.create () in
      Lwt.async (attach_output_thread flow output) ;
      Single {flow ;  input = input_mvar ; output}

    let attach_multi flow telnet =
      all := M.add flow {flow ;
                         input = global_input_queue ;
                         output = global_screen } !all ;
      Logs.info (fun m -> m "new client. %d multiplayers connected."
                    (M.cardinal !all)) ;
      Lwt.async (attach_output_thread flow global_screen);
      attach_input_listener flow telnet global_input_queue ()

    let stop_playing conn = match conn with
      | Single {flow ; _ }->
        Logs.warn (fun m -> m "%s" __LOC__);
        detach flow
      | Multi ->
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
        let text =
          (* aka String.rtrim, that would have ben nice: *)
          String.to_seq text |> List.of_seq |> fun chars ->
          List.fold_right (fun (ch:char) (drop_ws, (acc:char list)) ->
              if drop_ws && ch = ' ' then true, acc else (false, ch::acc))
            chars (true, (['\r';'\n']:char list))
          |> snd |> List.to_seq |> String.of_seq in
        write_string flow text >>= fun () ->
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
    (* todo run in batches of e.g. 1000 instructions *)
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
    and session_start kv conn =
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
      Player.attach_multi flow telnet
    in
    let port = Key_gen.port () in
    if Key_gen.multiplayer () then begin
      multiplayer := true
    end ;
    Logs.info (fun m -> m "Listening on [%a:%d]"
                  Fmt.(list Ipaddr.V4.pp) (S.(IPV4.get_ip @@ ipv4 s)) port);
    S.listen_tcpv4 s ~port (fun flow ->
        log_new flow ;
        let telnet, out = Server.init () in
        (KV.get kv (Mirage_kv.Key.v "MOTD.txt") >|= Rresult.R.get_ok) >>= fun motd ->
        let out_motd = Server.encode (Cstruct.of_string motd) in
        S.TCPV4.writev flow [out; out_motd] >>= function
        | Error e ->
          log_write_err flow e; S.TCPV4.close flow
        | Ok () ->
          if !multiplayer
          then begin
            if not @@ Player.is_connected () then
              Lwt.async (fun () ->
                  session_start kv (Player.multi_connection)) ;
            broadcast
              (Fmt.strf
                 "You \x1b[33mfeel\x1b[0m a disturbance\x1b[0m in the force. \
                  It feels like \x1b[33m%a\x1b[0m\r\n"
                 Fmt.(pair ~sep:(unit":") Ipaddr.V4.pp int) (S.TCPV4.dst flow))
            >>= fun () -> multiplayer_attach flow telnet
          end else session_start kv (Player.attach_single flow telnet)
      );
    S.listen s
end
