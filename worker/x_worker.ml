open Js_of_ocaml
open Js_of_ocaml.Js
module Merlin_worker = Worker

let respond m = Js_of_ocaml.Worker.post_message (X_protocol.resp_to_bytes m)

let reformat ~id code =
  let code' =
    try Ocamlfmt.fmt code
    with _err ->
      (* Brr.Console.log [ "ocamlformat error"; Printexc.to_string _err ]; *)
      code
  in
  if code <> code' then respond (Formatted_source (id, code'));
  code'

(* ✅ Add this helper function to send stdout/stderr output *)
let post_io_message kind content =
  let json = _object [|

   ("type", string kind);
  ("content", string content)
|]in
  Js_of_ocaml.Worker.postMessage json

(* ✅ Modify your run function to set up the flushers *)
let run () =
  (* 👇 Add this block at the top of the run function *)
  Sys_js.set_channel_flusher stdout (fun s -> post_io_message "stdout" s);
  Sys_js.set_channel_flusher stderr (fun s -> post_io_message "stderr" s);

  Js_of_ocaml.Worker.set_onmessage @@ fun marshaled_message ->
  match X_protocol.req_of_bytes marshaled_message with
  | Merlin (id, action) ->
      respond (Merlin_response (id, Merlin_worker.on_message action))
  | Format (id, code) -> ignore (reformat ~id code : string)
  | Eval (id, code) ->
      let code = reformat ~id code in
      let output ~loc out = respond (Top_response_at (id, loc, out)) in
      let result = Eval.execute ~output ~id code in
      respond (Top_response (id, result))
  | Setup -> Eval.setup_toplevel ()
