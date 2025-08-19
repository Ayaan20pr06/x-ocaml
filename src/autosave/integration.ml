open Lwt.Syntax
open Js_of_ocaml
open Dom_html  (* Add this for console access *)
open Autosave_types
open Autosave_storage
open Autosave_manager
open Autosave_restore


let console = Js.Unsafe.global##.console


(* Module type for the public interface of the autosave extension *)
module type XOcaml_AutoSave_Type = sig
  type t = {
    storage: AutoSave_Storage.t;
    manager: AutoSave_Manager.t option ref;
    element: Dom_html.element Js.t;
    element_id: string;
    document_id: string;
  }
  
  val create : Dom_html.element Js.t -> t storage_result
  val initialize_autosave : t -> unit Lwt.t
  val save_now : t -> unit storage_result
  val show_restore_dialog : t -> unit Lwt.t
  val destroy : t -> unit
end

(* Global registry to track autosave instances *)
module AutoSave_Registry = struct
  let instances : (string, (module XOcaml_AutoSave_Type with type t = 'a)) Hashtbl.t = Hashtbl.create 10
  
  let register element_id instance =
    Hashtbl.replace instances element_id instance
  
  let find element_id =
    Hashtbl.find_opt instances element_id
  
  let remove element_id =
    Hashtbl.remove instances element_id
    
  let find_by_element element =
    let element_id = Js.to_string element##.id in
    if element_id = "" then None
    else find element_id
end

(* Implementation of the autosave extension *)
module XOcaml_AutoSave : XOcaml_AutoSave_Type = struct
  type t = {
    storage: AutoSave_Storage.t;
    manager: AutoSave_Manager.t option ref;
    element: Dom_html.element Js.t;
    element_id: string;
    document_id: string;
  }

  (* Generate deterministic document ID based on element position and content *)
  let generate_document_id element =
    let rec get_element_path el path =
      match Js.Opt.to_option el##.parentElement with
      | None -> path
      | Some parent ->
          let siblings = parent##.children in
          let rec find_index i =
            if i >= siblings##.length then 0
            else
              match Js.Opt.to_option (siblings##item i) with
              | Some sibling when Js.to_string sibling##.id = Js.to_string el##.id -> i
              | _ -> find_index (i + 1)
          in
          let index = find_index 0 in
          get_element_path parent ((Js.to_string el##.tagName, index) :: path)
    in
    
    (* Get element path in DOM *)
    let path = get_element_path element [] in
    let path_str = path |> List.map (fun (tag, idx) ->
      Printf.sprintf "%s[%d]" tag idx
    ) |> String.concat "/" in
    
    (* Include initial content hash for uniqueness *)
    let initial_content = Js.to_string element##.textContent in
    let content_hash = Hashtbl.hash initial_content in
    
    (* Generate deterministic ID *)
    Printf.sprintf "doc_%s_%x"
      (Digest.to_hex (Digest.string path_str))
      content_hash

  
  let get_or_create_document_id element storage =
    
    let get_attr name = element##getAttribute (Js.string name) |> Js.Opt.to_option in
    
    match get_attr "document-id" with
    | Some attr when Js.to_string attr <> "" ->
        Lwt.return (Js.to_string attr)
    | _ ->
      
      match get_attr "data-block-id" with
      | Some attr when Js.to_string attr <> "" ->
        Lwt.return (Js.to_string attr)
      | _ ->
        
        let generated_id = generate_document_id element in
        let* existing = AutoSave_Storage.load_document storage generated_id in
        match existing with
        | Ok _ ->
          
          element##setAttribute (Js.string "document-id") (Js.string generated_id);
          Lwt.return generated_id
        | Error _ ->
          
          element##setAttribute (Js.string "document-id") (Js.string generated_id);
          Lwt.return generated_id

  let create element =
    let* storage_result = AutoSave_Storage.create () in
    match storage_result with
    | Error e ->
      
      ignore (console##error (Js.string "Failed to initialize storage"));
      Lwt.return (Error e)
    | Ok storage ->
      
      let element_id =
        if Js.to_string element##.id = "" then
          let new_id = Printf.sprintf "x-ocaml-%d" (Random.int 1000000) in
          element##.id := Js.string new_id;
          new_id
        else
          Js.to_string element##.id
      in
      
      (* Get or create persistent document ID *)
      let* document_id = get_or_create_document_id element storage in
      
      let instance = {
        storage;
        manager = ref None;
        element;
        element_id;
        document_id;
      } in
      
      Lwt.return (Ok instance)

  (* Create visual status indicator *)
  let create_status_indicator element =
    (* Check if status indicator already exists *)
    let existing = element##querySelector (Js.string ".autosave-status") in
    match Js.Opt.to_option existing with
    | Some indicator -> indicator
    | None ->
        let status_indicator = Dom_html.createDiv Dom_html.document in
        status_indicator##.className := Js.string "autosave-status";
        status_indicator##.innerHTML := Js.string
          "<span class='status-icon'>●</span><span class='status-text'></span>";
        
        
        let style_id = "autosave-styles" in
        if Js.Opt.test (Dom_html.document##getElementById (Js.string style_id)) = false then
          let style = Dom_html.createStyle Dom_html.document in
          style##.id := Js.string style_id;
          style##.textContent := Js.some (Js.string {|
            .autosave-status {
              position: absolute;
              top: 5px;
              right: 5px;
              font-size: 12px;
              padding: 2px 8px;
              border-radius: 3px;
              background: rgba(0,0,0,0.1);
              transition: all 0.3s ease;
              z-index: 1000;
            }
            .autosave-status.saved { color: #4CAF50; background: rgba(76,175,80,0.1); }
            .autosave-status.saving { color: #2196F3; background: rgba(33,150,243,0.1); }
            .autosave-status.error { color: #f44336; background: rgba(244,67,54,0.1); }
            .autosave-status.idle { opacity: 0.5; }
            .autosave-status .status-text { margin-left: 4px; font-size: 11px; }
          |});
          Dom.appendChild Dom_html.document##.head style;
        
        
        element##.style##.position := Js.string "relative";
        
        Dom.appendChild element status_indicator;
        status_indicator

  
  let update_status_indicator indicator status text =
    indicator##.className := Js.string ("autosave-status " ^ status);
    Js.Opt.iter (indicator##querySelector (Js.string ".status-text")) (fun text_span ->
      text_span##.textContent := Js.some (Js.string text)
    )

  
  let initialize_autosave autosave_ext =
    (* Check if auto-save is enabled via attribute *)
    let auto_save_attr = autosave_ext.element##getAttribute (Js.string "auto-save") in
    let auto_save_enabled = match Js.Opt.to_option auto_save_attr with
      | Some attr -> Js.to_string attr <> "false"
      | None -> true  (* Default to enabled *)
    in
    
    if not auto_save_enabled then
      Lwt.return_unit
    else
      
      let get_int_attr name default =
        match Js.Opt.to_option (autosave_ext.element##getAttribute (Js.string name)) with
        | Some attr ->
            (try int_of_string (Js.to_string attr)
              with _ -> default)
        | None -> default
      in
      
      let get_bool_attr name default =
        match Js.Opt.to_option (autosave_ext.element##getAttribute (Js.string name)) with
        | Some attr -> Js.to_string attr = "true"
        | None -> default
      in
      
      (* Get default config and override with attributes *)
      let default_config = AutoSave_Manager.get_default_config () in
      let config = {
        default_config with
        save_delay_ms = get_int_attr "save-delay" default_config.save_delay_ms;
        periodic_interval_ms = get_int_attr "periodic-interval" default_config.periodic_interval_ms;
        max_versions = get_int_attr "max-versions" default_config.max_versions;
        save_on_blur = get_bool_attr "save-on-blur" default_config.save_on_blur;
        save_execution_state = get_bool_attr "save-execution-state" default_config.save_execution_state;
      } in
      
      (* Creating status indicator *)
      let status_indicator = create_status_indicator autosave_ext.element in
      update_status_indicator status_indicator "idle" "";
      
      (* Creating auto-save manager *)
      let manager = AutoSave_Manager.create
        autosave_ext.storage
        autosave_ext.document_id
        ~config
        ~on_save_success:(fun () ->
          update_status_indicator status_indicator "saved" "Saved";
          let _ = Dom_html.setTimeout
            (fun () ->
              update_status_indicator status_indicator "idle" "")
            2000.0
          in ())
        ~on_save_error:(fun error ->
          let error_msg = error_to_string error in
          update_status_indicator status_indicator "error" error_msg)
        ~on_state_change:(fun has_changes ->
          if has_changes then
            update_status_indicator status_indicator "saving" "..."
          else
            update_status_indicator status_indicator "idle" "")
        ()
      in
      
      autosave_ext.manager := Some manager;
      
      
      let _ = AutoSave_Manager.setup_event_listeners manager in
      
      
      let* restore_result = AutoRestore.auto_restore_session
        autosave_ext.storage
        autosave_ext.document_id
      in
      
      (match restore_result with
      | (Some _, _) ->
          update_status_indicator status_indicator "saved" "Restored";
          let _ = Dom_html.setTimeout
            (fun () -> update_status_indicator status_indicator "idle" "")
            3000.0
          in ()
      | _ -> ());
      
      Lwt.return_unit

  
  let save_now autosave_ext =
    match !(autosave_ext.manager) with
    | None -> Lwt.return (Error (Database_error "Auto-save not initialized"))
    | Some manager -> AutoSave_Manager.save_now manager

  
  let show_restore_dialog autosave_ext =
    let* result = AutoRestore.show_restore_dialog autosave_ext.storage in
    match result with
    | Some document_id ->
        
        ignore (console##log (Js.string ("Restore selected: " ^ document_id)));
        Lwt.return_unit
    | None ->
        Lwt.return_unit

  
  let destroy autosave_ext =
    (match !(autosave_ext.manager) with
    | None -> ()
    | Some manager -> AutoSave_Manager.destroy manager)
end

(* JavaScript API *)
let () =
  Js.export "XOcamlAutoSave" (object%js
    val create = fun element ->
      Lwt.async (fun () ->
        let* result = XOcaml_AutoSave.create element in
        match result with
        | Ok autosave_ext ->
            let* _ = XOcaml_AutoSave.initialize_autosave autosave_ext in
            Lwt.return_unit
        | Error e -> 
            (* Fixed: Use Dom_html.console with ignore *)
            ignore (console##error (Js.string ("AutoSave creation failed: " ^ (error_to_string e))));
            Lwt.return_unit);
      Js.null

    val saveNow = fun element ->
      Lwt.async (fun () ->
        let element_id = Js.to_string element##.id in
        ignore (console##log (Js.string ("Manual save requested for: " ^ element_id)));
        Lwt.return_unit);
      Js.null

    val showRestoreDialog = fun element ->
      Lwt.async (fun () ->
        let element_id = Js.to_string element##.id in
        ignore (console##log (Js.string ("Restore dialog requested for: " ^ element_id)));
        Lwt.return_unit);
      Js.null

    val destroy = fun element ->
      let element_id = Js.to_string element##.id in
      ignore (console##log (Js.string ("Destroy requested for: " ^ element_id)))
      
    val getStatus = fun element ->
      let element_id = Js.to_string element##.id in
      ignore (console##log (Js.string ("Status requested for: " ^ element_id)));
      Js.Unsafe.obj [|
        ("enabled", Js.Unsafe.inject (Js.bool true));
        ("hasUnsavedChanges", Js.Unsafe.inject (Js.bool false));
        ("lastSaveTime", Js.Unsafe.inject (Js.number_of_float 0.0));
        ("version", Js.Unsafe.inject (Js.number_of_float 1.0));
        ("saveInProgress", Js.Unsafe.inject (Js.bool false));
      |]
  end)