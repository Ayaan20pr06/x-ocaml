open Brr
open Brr_io
open Lwt

(* Irmin store setup *)
module Store = Irmin_git.Generic(Irmin_indexeddb.Content_store)(Irmin_indexeddb.Branch_store)(Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)

let init_store () =
  let config = Irmin_indexeddb.config ~name:"x-ocaml-store" () in
  Store.Repo.v config >>= Store.main

let save_code store id content =
  let key = ["codeblocks"; string_of_int id] in
  let page_key = ["page_hash"] in
  let page_content = El.to_jstr (Document.to_el G.document) in
  let hash = Digest.to_hex (Digest.string (Jstr.to_string page_content)) in
  Store.set_exn store key content ~info:(Store.Info.empty) >>= fun () ->
  Store.set_exn store page_key hash ~info:(Store.Info.empty)

let restore_code store id =
  let key = ["codeblocks"; string_of_int id] in
  let page_key = ["page_hash"] in
  Store.find store page_key >>= fun saved_hash_opt ->
  let current_hash = Digest.to_hex (Digest.string (Jstr.to_string (El.to_jstr (Document.to_el G.document)))) in
  match saved_hash_opt with
  | Some saved_hash when saved_hash = current_hash ->
      Store.find store key
  | _ ->
      Lwt.return_none

let all : Cell.t list ref = ref []
let find_by_id id = List.find (fun t -> Cell.id t = id) !all

let current_script =
  El.of_jv (Jv.get (Document.to_jv G.document) "currentScript")

let current_attribute attr = El.at (Jstr.v attr) current_script

let extra_load =
  match current_attribute "src-load" with
  | None -> None
  | Some url -> Some (Jstr.to_string url)

let worker_url =
  match current_attribute "src-worker" with
  | None -> failwith "x-ocaml script missing src-worker attribute"
  | Some url -> Jstr.to_string url

let worker = Client.make ?extra_load worker_url

let () =
  Client.on_message worker @@ function
  | Formatted_source (id, code_fmt) -> Cell.set_source (find_by_id id) code_fmt
  | Top_response_at (id, loc, msg) -> Cell.add_message (find_by_id id) loc msg
  | Top_response (id, msg) -> Cell.completed_run (find_by_id id) msg
  | Merlin_response (id, msg) -> Cell.receive_merlin (find_by_id id) msg

let () = Client.post worker Setup

let elt_name =
  match current_attribute "elt-name" with
  | None -> Jstr.v "x-ocaml"
  | Some name -> name

let _ =
  init_store () >>= fun store ->
  Webcomponent.define elt_name @@ fun this ->
  let prev = match !all with [] -> None | e :: _ -> Some e in
  let id = List.length !all in
  let editor = Cell.init ~id ~store worker this in (* Updated to include ~store *)
  all := editor :: !all;
  Cell.set_prev ~prev editor;
  (* Restore saved content on initialization *)
  restore_code store id >>= fun content_opt ->
  (match content_opt with
   | Some content -> Cell.set_source editor content
   | None -> ());
  Lwt.return_unit