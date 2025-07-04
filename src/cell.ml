open Brr
open Brr_io
open Lwt

(* Reference save_code and restore_code from x_ocaml.ml *)
external save_code : Store.t -> int -> string -> unit Lwt.t = "save_code"
external restore_code : Store.t -> int -> string option Lwt.t = "restore_code"

type status = Not_run | Running | Run_ok | Request_run

type t = {
  id : int;
  mutable prev : t option;
  mutable next : t option;
  mutable status : status;
  cm : Editor.t;
  worker : Client.t;
  merlin_worker : Merlin_ext.Client.worker;
  store : Store.t; (* Added for Irmin store *)
}

let id t = t.id

let pre_source t =
  let rec go acc t =
    match t.prev with
    | None -> String.concat "\n" (List.rev acc)
    | Some e -> go (Editor.source e.cm :: acc) e
  in
  let s = go [] t in
  if s = "" then s else s ^ " ;;\n"

let rec invalidate_from ~editor =
  editor.status <- Not_run;
  Editor.clear editor.cm;
  let count = Editor.nb_lines editor.cm in
  match editor.next with
  | None -> ()
  | Some editor ->
      Editor.set_previous_lines editor.cm count;
      invalidate_from ~editor

let invalidate_after ~editor =
  editor.status <- Not_run;
  let count = Editor.nb_lines editor.cm in
  match editor.next with
  | None -> ()
  | Some editor ->
      Editor.set_previous_lines editor.cm count;
      invalidate_from ~editor

let rec refresh_lines_from ~editor =
  let count = Editor.nb_lines editor.cm in
  match editor.next with
  | None -> ()
  | Some editor ->
      Editor.set_previous_lines editor.cm count;
      refresh_lines_from ~editor

let rec run editor =
  if editor.status = Running then ()
  else (
    editor.status <- Request_run;
    Editor.clear_messages editor.cm;
    match editor.prev with
    | Some e when e.status <> Run_ok -> run e
    | _ ->
        editor.status <- Running;
        let code_txt = Editor.source editor.cm in
        Client.eval ~id:editor.id editor.worker code_txt)

let set_prev ~prev t =
  let () = match t.prev with None -> () | Some prev -> prev.next <- None in
  t.prev <- prev;
  match prev with
  | None ->
      Editor.set_previous_lines t.cm 0;
      refresh_lines_from ~editor:t;
      run t
  | Some p ->
      assert (p.next = None);
      p.next <- Some t;
      refresh_lines_from ~editor:p;
      run t

let set_source_from_html editor this =
  let doc = Webcomponent.text_content this in
  let doc = String.trim doc in
  Editor.set_source editor.cm doc;
  invalidate_from ~editor;
  Client.fmt ~id:editor.id editor.worker doc

let init ~id ~store worker this =
  let shadow = Webcomponent.attach_shadow this in

  El.append_children shadow
    [ El.style [ El.txt @@ Jstr.v [%blob "style.css"] ] ];
  let run_btn = El.button [ El.txt (Jstr.v "Run") ] in
  let save_btn = El.button [ El.txt (Jstr.v "Save") ] in
  let restore_btn = El.button [ El.txt (Jstr.v "Restore") ] in
  El.append_children shadow
    [ El.div ~at:[ At.class' (Jstr.v "run_btn") ] [ run_btn; save_btn; restore_btn ] ];

  let cm = Editor.make shadow in

  let merlin = Merlin_ext.make ~id worker in
  let merlin_worker = Merlin_ext.Client.make_worker merlin in
  let editor =
    {
      id;
      status = Not_run;
      cm;
      prev = None;
      next = None;
      worker;
      merlin_worker;
      store;
    }
  in
  Editor.on_change cm (fun () -> invalidate_after ~editor);
  set_source_from_html editor this;

  (* Restore saved content on initialization *)
  restore_code store id >>= fun content_opt ->
  (match content_opt with
   | Some content ->
       Editor.set_source editor.cm content;
       invalidate_from ~editor;
       Client.fmt ~id:editor.id editor.worker content
   | None -> Lwt.return_unit) >>= fun () ->

  (* Event listeners for buttons *)
  let _ : Ev.listener =
    Ev.listen Ev.click (fun _ev -> run editor) (El.as_target run_btn)
  in
  let _ : Ev.listener =
    Ev.listen Ev.click (fun _ev ->
      let content = Editor.source editor.cm in
      save_code store id content
    ) (El.as_target save_btn)
  in
  let _ : Ev.listener =
    Ev.listen Ev.click (fun _ev ->
      restore_code store id >>= fun content_opt ->
      (match content_opt with
       | Some content ->
           Editor.set_source editor.cm content;
           invalidate_from ~editor;
           Client.fmt ~id:editor.id editor.worker content
       | None -> ());
      Lwt.return_unit
    ) (El.as_target restore_btn)
  in

  Merlin_ext.set_context merlin (fun () -> pre_source editor);
  Editor.configure_merlin cm (Merlin_ext.extensions merlin_worker);

  let _ : Mutation_observer.t =
    Mutation_observer.observe ~target:(Webcomponent.as_target this)
    @@ Mutation_observer.create (fun _ _ -> set_source_from_html editor this)
  in

  editor

let set_source editor doc =
  Editor.set_source editor.cm doc;
  refresh_lines_from ~editor

let render_message msg =
  let kind, text =
    match msg with
    | X_protocol.Stdout str -> ("stdout", str)
    | Stderr str -> ("stderr", str)
    | Meta str -> ("meta", str)
  in
  El.pre
    ~at:[ At.class' (Jstr.v ("caml_" ^ kind)) ]
    [ El.txt (Jstr.v text) ]

let add_message t loc msg =
  Editor.add_message t.cm loc (List.map render_message msg)

let completed_run ed msg =
  (if msg <> [] then
     let loc = String.length (Editor.source ed.cm) in
     add_message ed loc msg);
  ed.status <- Run_ok;
  match ed.next with Some e when e.status = Request_run -> run e | _ -> ()

let receive_merlin t msg =
  Merlin_ext.Client.on_message t.merlin_worker
    (Merlin_ext.fix_answer (pre_source t) msg)
