open Lwt.Syntax
open Js_of_ocaml
open Dom_html  
open Autosave_types
open Autosave_storage

(* console binding *)
let console = Js.Unsafe.global##.console

module AutoRestore = struct
  
  (* Loading document state from storage - only data returning, no DOM *)
  let load_document_state storage document_id =
    AutoSave_Storage.load_document storage document_id
  
  
  let load_execution_state storage document_id =
    AutoSave_Storage.load_execution_state storage document_id
  
  (* Auto-restore session - takes data but doesn't apply it *)
  let auto_restore_session storage document_id =
    let* doc_result = load_document_state storage document_id in
    let* exec_result = load_execution_state storage document_id in
    
    let doc_state_opt = match doc_result with
      | Ok doc_state -> 
          
          ignore (console##log (Js.string (Printf.sprintf "Loaded document state for: %s" document_id)));
          Some doc_state
      | Error Not_found ->
          ignore (console##log (Js.string (Printf.sprintf "No saved document found for: %s" document_id)));
          None
      | Error e ->
          ignore (console##error (Js.string (Printf.sprintf "Failed to load document for %s: %s" document_id (error_to_string e))));
          None
    in
    
    let exec_state_opt = match exec_result with
      | Ok exec_state ->
          ignore (console##log (Js.string (Printf.sprintf "Loaded execution state for: %s" document_id)));
          Some exec_state
      | Error Not_found ->
          ignore (console##log (Js.string (Printf.sprintf "No saved execution state for: %s" document_id)));
          None
      | Error e ->
          ignore (console##error (Js.string (Printf.sprintf "Failed to load execution state for %s: %s" document_id (error_to_string e))));
          None
    in
    
    Lwt.return (doc_state_opt, exec_state_opt)
  
  (* Creating and showing restore dialog - returns selected document ID *)
  let show_restore_dialog storage =
    let* docs_result = AutoSave_Storage.list_documents storage ~limit:20 () in
    match docs_result with
    | Error e ->
        ignore (console##error (Js.string ("Failed to load documents for restore dialog: " ^ (error_to_string e))));
        Lwt.return None
    | Ok [] ->
        let alert_dialog = Dom_html.createDiv Dom_html.document in
        alert_dialog##.innerHTML := Js.string 
          "<div class='autosave-alert'>No saved sessions found.</div>";
        Dom.appendChild Dom_html.document##.body alert_dialog;
        
        let _ = Dom_html.setTimeout 
          (fun () -> 
            try Dom.removeChild Dom_html.document##.body alert_dialog
            with _ -> ())
          3000.0
        in
        Lwt.return None
    | Ok docs ->
        
        let (result_promise, result_resolver) = Lwt.wait () in
        
        let dialog = Dom_html.createDiv Dom_html.document in
        dialog##.className := Js.string "autosave-restore-dialog";
        
        let html_content = Buffer.create 2048 in
        Buffer.add_string html_content {|
          <div class="dialog-header">
            <h3>Restore Previous Session</h3>
            <button class="close-btn-header" title="Close">&times;</button>
          </div>
          <div class="session-list">
        |};
        
        List.iteri (fun i doc ->
          let date_str = 
            
            let date = new%js Js.date_fromTimeValue (Js.number_of_float doc.timestamp) in
            Js.to_string date##toLocaleString in
          
          let preview_content = 
            let content = doc.content in (* doc is now correctly typed as document_state *)
            let max_len = 100 in
            let preview = if String.length content > max_len then
              String.sub content 0 max_len ^ "..."
            else content in
            (* HTML escaping *)
            let escaped = Bytes.of_string preview in
            for j = 0 to Bytes.length escaped - 1 do
              match Bytes.get escaped j with
              | '<' -> Bytes.set escaped j '['
              | '>' -> Bytes.set escaped j ']'
              | '&' -> Bytes.set escaped j '+'
              | '"' -> Bytes.set escaped j '\''
              | _ -> ()
            done;
            Bytes.to_string escaped in
          
          Buffer.add_string html_content 
            (Printf.sprintf {|
              <div class="session-item" data-doc-id="%s">
                <div class="session-info">
                  <div class="session-title">
                    <strong>Session %d</strong>
                    <span class="session-version">v%d</span>
                  </div>
                  <div class="session-date">%s</div>
                  <div class="content-preview">%s</div>
                </div>
                <button class="restore-btn" data-doc-id="%s">Restore</button>
              </div>
            |} doc.id (i + 1) doc.version date_str preview_content doc.id)
        ) docs;
        
        Buffer.add_string html_content "</div>";
        dialog##.innerHTML := Js.string (Buffer.contents html_content);
        
        (* Adding dialog styles *)
        let style_id = "autosave-dialog-styles" in
        if Js.Opt.test (Dom_html.document##getElementById (Js.string style_id)) = false then begin
          let style = Dom_html.createStyle Dom_html.document in
          style##.id := Js.string style_id;
          style##.innerHTML := Js.string {|
            .autosave-restore-dialog {
              position: fixed;
              top: 50%;
              left: 50%;
              transform: translate(-50%, -50%);
              background: white;
              border: 1px solid #ccc;
              border-radius: 8px;
              padding: 20px;
              max-width: 600px;
              max-height: 80vh;
              overflow-y: auto;
              z-index: 10000;
              box-shadow: 0 4px 20px rgba(0,0,0,0.1);
            }
            .dialog-header {
              display: flex;
              justify-content: space-between;
              align-items: center;
              margin-bottom: 20px;
            }
            .dialog-header h3 {
              margin: 0;
            }
            .close-btn-header {
              background: none;
              border: none;
              font-size: 24px;
              cursor: pointer;
              padding: 0;
              width: 30px;
              height: 30px;
            }
            .session-item {
              border: 1px solid #eee;
              padding: 15px;
              margin-bottom: 10px;
              border-radius: 4px;
              display: flex;
              justify-content: space-between;
              align-items: center;
            }
            .session-item:hover {
              background-color: #f5f5f5;
            }
            .session-info {
              flex: 1;
            }
            .session-title {
              font-weight: bold;
              margin-bottom: 5px;
            }
            .session-version {
              color: #666;
              font-size: 12px;
              margin-left: 10px;
            }
            .session-date {
              color: #888;
              font-size: 14px;
              margin-bottom: 5px;
            }
            .content-preview {
              color: #666;
              font-size: 12px;
              font-family: monospace;
              white-space: nowrap;
              overflow: hidden;
              text-overflow: ellipsis;
            }
            .restore-btn {
              background-color: #007bff;
              color: white;
              border: none;
              padding: 8px 16px;
              border-radius: 4px;
              cursor: pointer;
              font-size: 14px;
            }
            .restore-btn:hover {
              background-color: #0056b3;
            }
            .autosave-alert {
              position: fixed;
              top: 20px;
              right: 20px;
              background: #f8f9fa;
              border: 1px solid #dee2e6;
              padding: 12px 20px;
              border-radius: 4px;
              box-shadow: 0 2px 10px rgba(0,0,0,0.1);
              z-index: 10001;
            }
          |};
          Dom.appendChild Dom_html.document##.head style
        end;
        
        Dom.appendChild Dom_html.document##.body dialog;
        
        (* close button *)
        let close_btn = dialog##querySelector (Js.string ".close-btn-header") in
        Js.Opt.iter close_btn (fun btn ->
          btn##.onclick := Dom_html.handler (fun _ ->
            Dom.removeChild Dom_html.document##.body dialog;
            Lwt.wakeup result_resolver None;
            Js._false
          )
        );
        
        (* restore buttons *)
        let restore_btns = dialog##querySelectorAll (Js.string ".restore-btn") in
        for i = 0 to restore_btns##.length - 1 do
          Js.Opt.iter (restore_btns##item i) (fun btn ->
            btn##.onclick := Dom_html.handler (fun _ ->
              let doc_id = Js.to_string (btn##getAttribute (Js.string "data-doc-id") |> Js.Opt.get) in
              Dom.removeChild Dom_html.document##.body dialog;
              Lwt.wakeup result_resolver (Some doc_id);
              Js._false
            )
          )
        done;
        
        result_promise
  
  (* all saved documents *)
  let list_saved_documents storage ?(limit=20) () =
    AutoSave_Storage.list_documents storage ~limit ()
  
  (* Delete a saved document and its execution state *)
  let delete_saved_session storage document_id =
    let* result = AutoSave_Storage.delete_document storage document_id in
    match result with
    | Ok () ->
        ignore (console##log (Js.string (Printf.sprintf "Deleted session: %s" document_id)));
        Lwt.return (Ok ())
    | Error e ->
        ignore (console##error (Js.string (Printf.sprintf "Failed to delete session %s: %s" document_id (error_to_string e))));
        Lwt.return (Error e)
  
  (* Clear all saved sessions *)
  let clear_all_sessions storage =
    let* docs_result = list_saved_documents storage ~limit:1000 () in
    match docs_result with
    | Error e -> Lwt.return (Error e)
    | Ok docs ->
        let* results = Lwt_list.map_p (fun doc ->
          delete_saved_session storage doc.id
        ) docs in
        
        let errors = List.filter_map (function Error e -> Some e | Ok () -> None) results in
        if errors = [] then
          Lwt.return (Ok ())
        else
          Lwt.return (Error (List.hd errors))
  
end