(* Fixed Autosave_manager.ml with proper js_of_ocaml compatibility *)
open Lwt.Syntax
open Js_of_ocaml
open Dom_html  (* Add this for console access *)
open Autosave_types
open Autosave_storage

(* Define console binding *)
let console = Js.Unsafe.global##.console

type save_trigger = 
  | Content_change
  | Cursor_move
  | Periodic_save
  | Focus_lost
  | Before_unload
  | Manual_save

type auto_save_config = {
  enabled: bool;
  save_delay_ms: int;
  periodic_interval_ms: int;
  max_versions: int;
  save_on_blur: bool;
  save_execution_state: bool;
  min_change_size: int;
  compression_enabled: bool;
}

let default_config = {
  enabled = true;
  save_delay_ms = 1000;
  periodic_interval_ms = 30000;
  max_versions = 50;
  save_on_blur = true;
  save_execution_state = true;
  min_change_size = 5;
  compression_enabled = false;
}

type manager_status = {
  enabled: bool;
  has_unsaved_changes: bool;
  last_save_time: float;
  version: int;
  save_in_progress: bool;
  last_error: storage_error option;
}

module AutoSave_Manager = struct
  type t = {
    storage: AutoSave_Storage.t;
    document_id: string;
    config: auto_save_config;
    mutable last_save_time: float;
    mutable has_unsaved_changes: bool;
    mutable save_timer: Dom_html.timeout_id option;
    mutable periodic_timer: Dom_html.timeout_id option;
    mutable last_content_hash: int;
    mutable last_content_length: int;
    mutable version_counter: int;
    mutable save_in_progress: bool;
    mutable last_error: storage_error option;
    (* Fixed: Use proper event type *)
    mutable event_listeners: (Dom_html.event Js.t -> bool Js.t) list;
    on_save_success: (unit -> unit) option;
    on_save_error: (storage_error -> unit) option;
    on_state_change: (bool -> unit) option;
  }

  let create storage document_id ?(config = default_config) ?on_save_success ?on_save_error ?on_state_change () =
    {
      storage;
      document_id;
      config;
      last_save_time = 0.0;
      has_unsaved_changes = false;
      save_timer = None;
      periodic_timer = None;
      last_content_hash = 0;
      last_content_length = 0;
      version_counter = 1;
      save_in_progress = false;
      last_error = None;
      event_listeners = [];
      on_save_success;
      on_save_error;
      on_state_change;
    }

  let hash_string s =
    let fnv_prime = 16777619 in
    let fnv_offset_basis = 2166136261 in
    let hash = ref fnv_offset_basis in
    String.iter (fun c ->
      hash := !hash lxor (Char.code c);
      hash := !hash * fnv_prime
    ) s;
    !hash

  let has_significant_change manager content =
    let content_length = String.length content in
    let length_diff = abs (content_length - manager.last_content_length) in
    length_diff >= manager.config.min_change_size

  let get_codemirror_instance element =
    try
      let shadow_root = Js.Unsafe.get element "shadowRoot" in
      let editor_container = shadow_root##querySelector (Js.string ".cm-editor") in
      match Js.Opt.to_option editor_container with
      | Some container -> 
          Some (Js.Unsafe.get container "CodeMirror")
      | None -> None
    with _ -> None

  let get_current_document_state manager =
    let x_ocaml_elements = Dom_html.document##querySelectorAll (Js.string "x-ocaml") in
    if x_ocaml_elements##.length = 0 then
      Error "No x-ocaml element found"
    else
      let rec find_element i =
        if i >= x_ocaml_elements##.length then None
        else
          match Js.Opt.to_option (x_ocaml_elements##item i) with
          | Some element ->
              let doc_id_attr = element##getAttribute (Js.string "document-id") in
              (match Js.Opt.to_option doc_id_attr with
               | Some attr when Js.to_string attr = manager.document_id ->
                   Some element
               | _ -> find_element (i + 1))
          | None -> find_element (i + 1)
      in
      
      match find_element 0 with
      | None -> Error "No matching x-ocaml element found"
      | Some element ->
          try
            let content = match get_codemirror_instance element with
              | Some cm -> Js.to_string (cm##getValue)
              | None -> 
                  (* Fixed: Handle optional textContent *)
                  match Js.Opt.to_option element##.textContent with
                  | Some s -> Js.to_string s
                  | None -> ""
            in
            
            let cursor_pos, selection_start, selection_end = 
              match get_codemirror_instance element with
              | Some cm ->
                  let cursor = cm##getCursor in
                  let pos = cursor##.ch + (cursor##.line * 10000) in
                  (try
                     let sel = cm##getSelection in
                     if Js.to_string sel = "" then (pos, pos, pos)
                     else
                       let sel_start = cm##getCursor (Js.string "start") in
                       let sel_end = cm##getCursor (Js.string "end") in
                       let start_pos = sel_start##.ch + (sel_start##.line * 10000) in
                       let end_pos = sel_end##.ch + (sel_end##.line * 10000) in
                       (pos, start_pos, end_pos)
                   with _ -> (pos, pos, pos))
              | None -> (0, 0, 0)
            in
            
            let scroll_top = match get_codemirror_instance element with
              | Some cm ->
                  (try
                     let scroll_info = cm##getScrollInfo in
                     scroll_info##.top
                   with _ -> 0)
              | None -> 0
            in
            
            Ok {
              id = manager.document_id;
              content;
              cursor_position = cursor_pos;
              selection_start;
              selection_end;
              scroll_top;
              timestamp = Js.to_float Js.date_now;  (* FIXED: Removed () *)
              version = manager.version_counter;
            }
          with exn ->
            Error (Printf.sprintf "Failed to extract document state: %s" (Printexc.to_string exn))

  let get_current_execution_state manager =
    let x_ocaml_elements = Dom_html.document##querySelectorAll (Js.string "x-ocaml") in
    if x_ocaml_elements##.length = 0 then
      {
        document_id = manager.document_id;
        toplevel_state = "";
        repl_history = [];
        execution_count = 0;
        timestamp = Js.to_float Js.date_now;  (* FIXED: Removed () *)
      }
    else
      match Js.Opt.to_option (x_ocaml_elements##item 0) with
      | None -> 
          {
            document_id = manager.document_id;
            toplevel_state = "";
            repl_history = [];
            execution_count = 0;
            timestamp = Js.to_float Js.date_now;  (* FIXED: Removed the invalid new%js Js.date_now *)
          }
      | Some x_ocaml_element ->
          let toplevel_state = try
            let state = Js.Unsafe.get x_ocaml_element "toplevelState" in
            Js.to_string state
          with _ -> ""
          in
          
          let repl_history = try
            let history = Js.Unsafe.get x_ocaml_element "replHistory" in
            Array.to_list (Js.to_array history) |> List.map Js.to_string
          with _ -> []
          in
          
          {
            document_id = manager.document_id;
            toplevel_state;
            repl_history;
            execution_count = List.length repl_history;
            timestamp = Js.to_float Js.date_now;  (* FIXED: Removed () *)
          }

  let string_of_trigger = function
    | Content_change -> "content_change"
    | Cursor_move -> "cursor_move"
    | Periodic_save -> "periodic_save"
    | Focus_lost -> "focus_lost"
    | Before_unload -> "before_unload"
    | Manual_save -> "manual_save"

  let perform_save manager trigger =
    if not manager.config.enabled || manager.save_in_progress then
      Lwt.return (Ok ())
    else begin
      manager.save_in_progress <- true;
      let current_time = Js.to_float Js.date_now in  (* FIXED: Removed () *)
      
      let save_operation =
        try%lwt
          match get_current_document_state manager with
          | Error msg ->
              let error = Serialization_error msg in
              manager.last_error <- Some error;
              Option.iter (fun f -> f error) manager.on_save_error;
              Lwt.return (Error error)
          | Ok doc_state ->
              let content_hash = hash_string doc_state.content in
              
              if content_hash = manager.last_content_hash && 
                 trigger <> Manual_save && trigger <> Before_unload then
                Lwt.return (Ok ())
              else if not (has_significant_change manager doc_state.content) && 
                      trigger = Content_change then
                Lwt.return (Ok ())
              else begin
                manager.version_counter <- manager.version_counter + 1;
                let updated_doc_state = { doc_state with version = manager.version_counter } in
                
                let* doc_result = AutoSave_Storage.save_document manager.storage updated_doc_state in
                
                let* exec_result = 
                  if manager.config.save_execution_state then
                    let exec_state = get_current_execution_state manager in
                    AutoSave_Storage.save_execution_state manager.storage exec_state
                  else
                    Lwt.return (Ok ())
                in
                
                match doc_result, exec_result with
                | Ok (), Ok () ->
                    manager.last_save_time <- current_time;
                    manager.has_unsaved_changes <- false;
                    manager.last_content_hash <- content_hash;
                    manager.last_content_length <- String.length doc_state.content;
                    manager.last_error <- None;
                    Option.iter (fun f -> f ()) manager.on_save_success;
                    Option.iter (fun f -> f false) manager.on_state_change;
                    
                    if trigger = Periodic_save then
                      Lwt.async (fun () ->
                        let* _ = AutoSave_Storage.cleanup_old_documents 
                          manager.storage ~keep_count:manager.config.max_versions () in
                        Lwt.return_unit);
                    
                    (* Fixed: Use Dom_html.console with ignore *)
                    ignore (console##log (Js.string 
                      (Printf.sprintf "Auto-save completed: %s (v%d)" 
                        (string_of_trigger trigger) manager.version_counter)));
                    Lwt.return (Ok ())
                | Error e, _ | _, Error e ->
                    manager.last_error <- Some e;
                    Option.iter (fun f -> f e) manager.on_save_error;
                    (* Fixed: Use Dom_html.console with ignore *)
                    ignore (console##error (Js.string 
                      (Printf.sprintf "Auto-save failed: %s" (error_to_string e))));
                    Lwt.return (Error e)
              end
        with exn ->
          let error = Serialization_error (Printexc.to_string exn) in
          manager.last_error <- Some error;
          Option.iter (fun f -> f error) manager.on_save_error;
          Lwt.return (Error error)
      in
      
      let* result = save_operation in
      manager.save_in_progress <- false;
      Lwt.return result
    end

  let schedule_save manager trigger =
    if not manager.config.enabled || manager.save_in_progress then () 
    else begin
      Option.iter Dom_html.clearTimeout manager.save_timer;
      manager.save_timer <- None;
      
      if not manager.has_unsaved_changes then begin
        manager.has_unsaved_changes <- true;
        Option.iter (fun f -> f true) manager.on_state_change;
      end;
      
      let delay = match trigger with
        | Manual_save | Before_unload -> 0.0
        | Focus_lost -> 100.0
        | _ -> float_of_int manager.config.save_delay_ms
      in
      
      let timer_id = Dom_html.setTimeout 
        (fun () ->
          manager.save_timer <- None;
          Lwt.async (fun () ->
            let* _ = perform_save manager trigger in
            Lwt.return_unit))
        delay
      in
      manager.save_timer <- Some timer_id
    end

  let save_now manager =
    Option.iter Dom_html.clearTimeout manager.save_timer;
    manager.save_timer <- None;
    perform_save manager Manual_save

  let setup_event_listeners manager =
    let handlers = ref [] in
    let x_ocaml_elements = Dom_html.document##querySelectorAll (Js.string "x-ocaml") in
    
    for i = 0 to x_ocaml_elements##.length - 1 do
      match Js.Opt.to_option (x_ocaml_elements##item i) with
      | None -> ()
      | Some element ->
          let change_handler = Dom_html.handler (fun _event ->
            schedule_save manager Content_change;
            Js._false
          ) in
          handlers := change_handler :: !handlers;
          element##addEventListener (Js.string "input") change_handler Js._false;
          element##addEventListener (Js.string "change") change_handler Js._false;
          
          let custom_change_handler = Dom_html.handler (fun _event ->
            schedule_save manager Content_change;
            Js._false
          ) in
          handlers := custom_change_handler :: !handlers;
          element##addEventListener (Js.string "x-ocaml-change") custom_change_handler Js._false;
          element##addEventListener (Js.string "x-ocaml-execute") custom_change_handler Js._false;
          
          if manager.config.save_on_blur then
            let blur_handler = Dom_html.handler (fun _event ->
              schedule_save manager Focus_lost;
              Js._false
            ) in
            handlers := blur_handler :: !handlers;
            element##addEventListener (Js.string "blur") blur_handler Js._false;
    done;
    
    Dom_html.window##.onbeforeunload := Dom_html.handler (fun _event ->
      if manager.has_unsaved_changes then begin
        ignore (perform_save manager Before_unload);
      end;
      Js._false
    );
    
    let visibility_handler = Dom_html.handler (fun _event ->
      if Dom_html.document##.hidden = Js._true && manager.has_unsaved_changes then
        schedule_save manager Focus_lost;
      Js._false
    ) in
    handlers := visibility_handler :: !handlers;
    Dom_html.document##addEventListener 
      (Js.string "visibilitychange") visibility_handler Js._false;
    
    if manager.config.periodic_interval_ms > 0 then
      let timer_id = Dom_html.setInterval
        (fun () ->
          if manager.has_unsaved_changes || manager.config.periodic_interval_ms <= 60000 then
            schedule_save manager Periodic_save)
        (float_of_int manager.config.periodic_interval_ms)
      in
      manager.periodic_timer <- Some timer_id;
    
    { manager with event_listeners = !handlers }

  let get_status manager = {
    enabled = manager.config.enabled;
    has_unsaved_changes = manager.has_unsaved_changes;
    last_save_time = manager.last_save_time;
    version = manager.version_counter;
    save_in_progress = manager.save_in_progress;
    last_error = manager.last_error;
  }

  let update_config manager new_config =
    if new_config.periodic_interval_ms <> manager.config.periodic_interval_ms then begin
      Option.iter Dom_html.clearInterval manager.periodic_timer;
      manager.periodic_timer <- None;
      
      if new_config.periodic_interval_ms > 0 then
        let timer_id = Dom_html.setInterval
          (fun () ->
            if manager.has_unsaved_changes then
              schedule_save manager Periodic_save)
          (float_of_int new_config.periodic_interval_ms)
        in
        manager.periodic_timer <- Some timer_id;
    end;
    
    { manager with config = new_config }

  let destroy manager =
    Option.iter Dom_html.clearTimeout manager.save_timer;
    Option.iter Dom_html.clearInterval manager.periodic_timer;
    manager.save_timer <- None;
    manager.periodic_timer <- None;
    ()
end

let get_default_config () = default_config