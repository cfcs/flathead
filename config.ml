open Mirage

let port =
  let doc = Key.Arg.info ~doc:"The TCP listen port." ["port"] in
  Key.(create "port" Arg.(opt int 23 doc))

let multiplayer : bool Mirage.Key.key =
  let doc = Key.Arg.info ~doc:"One shared global state"
      ["multiplayer"] in
  Key.(create "multiplayer" Arg.(opt bool false doc))

let stories = generic_kv_ro "stories"

let main =
  let packages = [ package "logs" ] in
  let keys = Key.[ abstract port; abstract multiplayer ] in
  foreign
    ~keys ~packages
    "Unikernel.Main" (kv_ro @-> stackv4 @-> job)

let stack = generic_stackv4 default_network

let () = register "flathead" [main $ stories $ stack ]
