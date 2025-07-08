open Brr
open Brr_io
open Lwt

(* Irmin store setup *)
module Store = Irmin_git.Generic
  (Irmin_indexeddb.Content_store)
  (Irmin_indexeddb.Branch_store)
  (Irmin.Contents.String)
  (Irmin.Path.String_list)
  (Irmin.Branch.String)

let init_store () =
  let config = Irmin_indexeddb.config ~name:"x-ocaml-store" () in
  Store.Repo.v config >>= Store.main

(* --- Stable Page Hashing --- *)
let compute_page_hash () =
  (* Only hash x-ocaml elements and their data-block-id attributes *)
  let code_blocks = Document.find_all (Jstr.v "x-ocaml") G.document in
  let ids =
    List.map (fun el ->
      match El.get_attribute (Jstr.v "data-block-id") el with
      | Some id -> Jstr.to_string id
      | None -> failwith "Missing data-block-id on x-ocaml element"
    ) code_blocks
  in
  let canonical = String.concat "," ids in
  Digest.to_hex (Digest.string canonical)

(* --- Save Code --- *)
let save_code store block_id content =
  let key = ["codeblocks"; block_id] in
  let page_key = ["page_hash"] in
  let hash = compute_page_hash () in
  Store.set_exn store key content ~info:(Store.Info.empty) >>= fun () ->
  Store.set_exn store page_key hash ~info:(Store.Info.empty)

(* --- Restore Code --- *)
let restore_code store block_id =
  let key = ["codeblocks"; block_id] in
  let page_key = ["page_hash"] in
  Store.find store page_key >>= fun saved_hash_opt ->
  let current_hash = compute_page_hash () in
  match saved_hash_opt with
  | Some saved_hash when saved_hash = current_hash ->
      Store.find store key
  | _ ->
      Lwt.return_none

(* --- Version History --- *)
let view_history store block_id =
  let key = ["codeblocks"; block_id] in
  Store.history store >>= fun history ->
  let commits = Store.History.fold (fun commit acc ->
    let info = Store.Commit.info commit in
    let date = Int64.to_string (Irmin.Info.date info) in
    let message = Irmin.Info.message info in
    (date, message) :: acc
  ) history []
  in
  Lwt.return commits

let restore_version store block_id commit_hash =
  let key = ["codeblocks"; block_id] in
  Store.of_commit_hash store commit_hash >>= function
  | None -> Lwt.return_none
  | Some commit -> Store.find ~commit store key

(* --- Branching --- *)
let create_branch store branch_name =
  Store.Branch.set store branch_name >>= fun () ->
  Lwt.return_unit

let switch_branch store branch_name =
  Store.of_branch store branch_name

(* --- Cell Management --- *)
let all : Cell.t list ref = ref []

let find_by_block_id block_id =
  List.find (fun t -> Cell.block_id t = block_id) !all

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
  | Formatted_source (block_id, code_fmt) -> Cell.set_source (find_by_block_id block_id) code_fmt
  | Top_response_at (block_id, loc, msg) -> Cell.add_message (find_by_block_id block_id) loc msg
  | Top_response (block_id, msg) -> Cell.completed_run (find_by_block_id block_id) msg
  | Merlin_response (block_id, msg) -> Cell.receive_merlin (find_by_block_id block_id) msg

let () = Client.post worker Setup

let elt_name =
  match current_attribute "elt-name" with
  | None -> Jstr.v "x-ocaml"
  | Some name -> name

let _ =
  init_store () >>= fun store ->
  Webcomponent.define elt_name @@ fun this ->
    let block_id =
      match El.get_attribute (Jstr.v "data-block-id") this with
      | Some id -> Jstr.to_string id
      | None -> failwith "x-ocaml element missing data-block-id attribute"
    in
    let prev = match !all with [] -> None | e :: _ -> Some e in
    let editor = Cell.init ~block_id ~store worker this in
    all := editor :: !all;
    Cell.set_prev ~prev editor;
    (* Restore saved content on initialization *)
    restore_code store block_id >>= function
    | Some content -> Cell.set_source editor content; Lwt.return_unit
    | None -> Lwt.return_unit
