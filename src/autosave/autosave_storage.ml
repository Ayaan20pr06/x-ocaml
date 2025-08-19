open Lwt.Syntax
open Js_of_ocaml
open Autosave_types


module IDB = Indexeddb.Idb_lwt
module Store = Indexeddb.Idb_lwt.Json

module AutoSave_Storage = struct
  type t = {
    db: IDB.db;
    db_name: string;
    version: int;
  }

  let database_name = "x_ocaml_autosave"
  let current_version = 2

  let documents_store_name = IDB.store_name "documents"
  let execution_states_store_name = IDB.store_name "execution_states" 
  let session_metadata_store_name = IDB.store_name "session_metadata"

  (* Database setup *)
  let setup_database ~old_version upgrader =
    if old_version < 1 then begin
      IDB.create_store upgrader documents_store_name 
        ~options:(IDB.store_options ~key_path:"id" ());
      IDB.create_store upgrader execution_states_store_name
        ~options:(IDB.store_options ~key_path:"document_id" ());
    end;
    
    if old_version < 2 then begin
      IDB.create_store upgrader session_metadata_store_name
        ~options:(IDB.store_options ~key_path:"document_id" ());
    end

  
  let create () : t storage_result =
    try%lwt
      let* db = IDB.make (IDB.db_name database_name) 
        ~version:current_version 
        ~init:setup_database in
      Lwt.return (Ok {db; db_name = database_name; version = current_version})
    with
    | exn -> 
        Lwt.return (Error (Database_error (Printexc.to_string exn)))

  
  let save_document (storage : t) (doc_state : document_state) : unit storage_result =
    try%lwt
      let store = Store.store storage.db documents_store_name in
      let json_obj = document_state_to_yojson doc_state in
      let* () = Store.set store (Js.string doc_state.id) json_obj in
      Lwt.return (Ok ())
    with
    | exn -> Lwt.return (Error (Database_error (Printexc.to_string exn)))

  
  let load_document (storage : t) (doc_id : string) : document_state storage_result =
    try%lwt
      let store = Store.store storage.db documents_store_name in
      let* result = Store.get store (Js.string doc_id) in
      match result with
      | Some json_obj ->
          (match document_state_of_yojson json_obj with
           | Ok doc -> Lwt.return (Ok doc)
           | Error msg -> Lwt.return (Error (Serialization_error msg)))
      | None -> Lwt.return (Error Not_found)
    with
    | exn -> Lwt.return (Error (Database_error (Printexc.to_string exn)))

  
  let save_execution_state (storage : t) (exec_state : execution_state) : unit storage_result =
    try%lwt
      let store = Store.store storage.db execution_states_store_name in
      let json_obj = execution_state_to_yojson exec_state in
      let* () = Store.set store (Js.string exec_state.document_id) json_obj in
      Lwt.return (Ok ())
    with
    | exn -> Lwt.return (Error (Database_error (Printexc.to_string exn)))

  
  let load_execution_state (storage : t) (doc_id : string) : execution_state storage_result =
    try%lwt
      let store = Store.store storage.db execution_states_store_name in
      let* result = Store.get store (Js.string doc_id) in
      match result with
      | Some json_obj ->
          (match execution_state_of_yojson json_obj with
           | Ok exec -> Lwt.return (Ok exec)
           | Error msg -> Lwt.return (Error (Serialization_error msg)))
      | None -> Lwt.return (Error Not_found)
    with
    | exn -> Lwt.return (Error (Database_error (Printexc.to_string exn)))

  
  let list_documents (storage : t) ?(limit = 50) () : document_state list storage_result =
    try%lwt
      let store = Store.store storage.db documents_store_name in
      let* all_bindings = Store.bindings store in
      
      
      let document_list : document_state list = List.filter_map (fun (_, json_obj) ->
        match document_state_of_yojson json_obj with
        | Ok (doc : document_state) -> Some doc
        | Error _ -> None
      ) all_bindings in
      
      
      let sorted_document_list : document_state list = List.sort 
        (fun (a : document_state) (b : document_state) -> 
          Float.compare b.timestamp a.timestamp) document_list in
      
      
      let limited_document_list : document_state list = take limit sorted_document_list in
      Lwt.return (Ok limited_document_list)
    with
    | exn -> Lwt.return (Error (Database_error (Printexc.to_string exn)))

  
  let delete_document (storage : t) (doc_id : string) : unit storage_result =
    try%lwt
      let docs_store = Store.store storage.db documents_store_name in
      let exec_store = Store.store storage.db execution_states_store_name in
      let meta_store = Store.store storage.db session_metadata_store_name in
      
      let* () = Store.remove docs_store (Js.string doc_id) in
      let* () = Store.remove exec_store (Js.string doc_id) in
      let* () = Store.remove meta_store (Js.string doc_id) in
      
      Lwt.return (Ok ())
    with
    | exn -> Lwt.return (Error (Database_error (Printexc.to_string exn)))

  (* Cleanup old documents *)
  let cleanup_old_documents (storage : t) ?(keep_count = 50) () : unit storage_result =
    let* docs_result = list_documents storage ~limit:(keep_count + 100) () in
    match docs_result with
    | Error e -> Lwt.return (Error e)
    | Ok (document_list : document_state list) ->
        if List.length document_list <= keep_count then
          Lwt.return (Ok ())
        else
          
          let sorted_documents : document_state list = List.sort 
            (fun (a : document_state) (b : document_state) -> 
              Float.compare b.timestamp a.timestamp) document_list in
          
          let documents_to_keep : document_state list = take keep_count sorted_documents in
          let documents_to_delete : document_state list = List.filter 
            (fun (doc : document_state) -> 
              not (List.exists (fun (keep_doc : document_state) -> 
                String.equal doc.id keep_doc.id) documents_to_keep)) document_list in
          
          let* delete_results = Lwt_list.map_s 
            (fun (doc : document_state) -> delete_document storage doc.id) documents_to_delete in
          
          let all_successful = List.for_all 
            (function Ok () -> true | Error _ -> false) delete_results in
          
          if all_successful then Lwt.return (Ok ())
          else Lwt.return (Error (Database_error "Some deletions failed"))

  (* Closing database connection *)  
  let close (storage : t) : unit =
    IDB.close storage.db
end

(* Helper functions *)
let rec take (n : int) (lst : 'a list) : 'a list = 
  match lst with
  | [] -> []
  | x :: xs when n > 0 -> x :: take (n - 1) xs
  | _ -> []