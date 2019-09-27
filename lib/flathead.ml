open Type;;
let story = Story.load "MiniZork.Z3" ;;
let screen = Screen.make (Character_height 50) (Character_width 80) ;;
let interpreter = Interpreter.make story screen ;;
let runner = Runner.make interpreter ;;
Runner.run runner ;;
(* let debugger = Debugger.make interpreter ;;
Debugger.run debugger ;; *)
