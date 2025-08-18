(* Autosave_types.ml - Data types and serialization for auto-save functionality *)

open Printf

(* Data types for auto-save functionality *)
type document_state = {
  id: string;
  content: string;
  cursor_position: int;
  selection_start: int;
  selection_end: int;
  scroll_top: int;
  timestamp: float;
  version: int;
} [@@deriving yojson, show]

type execution_state = {
  document_id: string;
  toplevel_state: string;
  repl_history: string list;
  execution_count: int;
  timestamp: float;
} [@@deriving yojson, show]

type session_metadata = {
  document_id: string;
  title: string;
  created_at: float;
  last_accessed: float;
  auto_save_enabled: bool;
} [@@deriving yojson, show]

type storage_error =
  | Database_error of string
  | Not_found
  | Serialization_error of string
  | Version_mismatch of int * int
[@@deriving yojson, show]

type 'a storage_result = ('a, storage_error) result Lwt.t

(* Migration support for future schema changes *)
type schema_version = int [@@deriving yojson, show]

let current_schema_version : schema_version = 1

type versioned_document = {
  schema_version: schema_version;
  document_data: document_state;
} [@@deriving yojson, show]

type versioned_execution = {
  schema_version: schema_version;
  execution_data: execution_state;
} [@@deriving yojson, show]

type versioned_metadata = {
  schema_version: schema_version;
  metadata_data: session_metadata;
} [@@deriving yojson, show]

(* Serialization functions using PPX-generated converters *)
let serialize_document (doc : document_state) : string =
  try
    doc |> document_state_to_yojson |> Yojson.Safe.to_string
  with exn ->
    eprintf "Error serializing document: %s\n" (Printexc.to_string exn);
    "{}"

let serialize_execution (exec : execution_state) : string =
  try
    exec |> execution_state_to_yojson |> Yojson.Safe.to_string
  with exn ->
    eprintf "Error serializing execution: %s\n" (Printexc.to_string exn);
    "{}"

let serialize_metadata (meta : session_metadata) : string =
  try
    meta |> session_metadata_to_yojson |> Yojson.Safe.to_string
  with exn ->
    eprintf "Error serializing metadata: %s\n" (Printexc.to_string exn);
    "{}"

(* Deserialization functions using PPX-generated converters *)
let deserialize_document (json_str : string) : document_state option =
  try
    let json = Yojson.Safe.from_string json_str in
    match document_state_of_yojson json with
    | Ok doc_state -> Some doc_state
    | Error msg ->
      eprintf "Deserialization error in document: %s (JSON: %s)\n"
        msg json_str;
      None
  with
  | Yojson.Json_error msg ->
    eprintf "JSON parsing error in document: %s\n" msg;
    None
  | exn ->
    eprintf "Unexpected error deserializing document: %s\n"
      (Printexc.to_string exn);
    None

let deserialize_execution (json_str : string) : execution_state option =
  try
    let json = Yojson.Safe.from_string json_str in
    match execution_state_of_yojson json with
    | Ok exec_state -> Some exec_state
    | Error msg ->
      eprintf "Deserialization error in execution: %s (JSON: %s)\n"
        msg json_str;
      None
  with
  | Yojson.Json_error msg ->
    eprintf "JSON parsing error in execution: %s\n" msg;
    None
  | exn ->
    eprintf "Unexpected error deserializing execution: %s\n"
      (Printexc.to_string exn);
    None

let deserialize_metadata (json_str : string) : session_metadata option =
  try
    let json = Yojson.Safe.from_string json_str in
    match session_metadata_of_yojson json with
    | Ok meta_data -> Some meta_data
    | Error msg ->
      eprintf "Deserialization error in metadata: %s (JSON: %s)\n"
        msg json_str;
      None
  with
  | Yojson.Json_error msg ->
    eprintf "JSON parsing error in metadata: %s\n" msg;
    None
  | exn ->
    eprintf "Unexpected error deserializing metadata: %s\n"
      (Printexc.to_string exn);
    None

(* Versioned serialization for future-proofing *)
let serialize_document_versioned (doc : document_state) : string =
  let versioned = { schema_version = current_schema_version; document_data = doc } in
  versioned |> versioned_document_to_yojson |> Yojson.Safe.to_string

let deserialize_document_versioned (json_str : string) : document_state option =
  try
    let json = Yojson.Safe.from_string json_str in
    match versioned_document_of_yojson json with
    | Ok versioned ->
      if versioned.schema_version = current_schema_version then
        Some versioned.document_data
      else (
        eprintf "Schema version mismatch: expected %d, got %d\n"
          current_schema_version versioned.schema_version;
        None )
    | Error msg ->
      eprintf "Deserialization error in versioned document: %s (JSON: %s)\n"
        msg json_str;
      None
  with exn ->
    eprintf "Error deserializing versioned document: %s\n"
      (Printexc.to_string exn);
    None

(* Utility functions for creating default states *)
let create_default_document ~id ~content () : document_state =
  {
    id;
    content;
    cursor_position = 0;
    selection_start = 0;
    selection_end = 0;
    scroll_top = 0;
    timestamp = Unix.time ();
    version = 1;
  }

let create_default_execution ~document_id () : execution_state =
  {
    document_id;
    toplevel_state = "";
    repl_history = [];
    execution_count = 0;
    timestamp = Unix.time ();
  }

let create_default_metadata ~document_id ~title () : session_metadata =
  {
    document_id;
    title;
    created_at = Unix.time ();
    last_accessed = Unix.time ();
    auto_save_enabled = true;
  }

(* Validation functions to ensure data integrity *)
let validate_document (doc : document_state) : (unit, string) result =
  if String.length doc.id = 0 then
    Error "Document ID cannot be empty"
  else if doc.cursor_position < 0 || doc.cursor_position > String.length doc.content then
    Error "Invalid cursor position"
  else if doc.selection_start < 0 || doc.selection_end < 0 ||
          doc.selection_start > String.length doc.content ||
          doc.selection_end > String.length doc.content then
    Error "Invalid selection range"
  else if doc.version < 1 then
    Error "Version must be positive"
  else
    Ok ()

let validate_execution (exec : execution_state) : (unit, string) result =
  if String.length exec.document_id = 0 then
    Error "Document ID cannot be empty"
  else if exec.execution_count < 0 then
    Error "Execution count cannot be negative"
  else
    Ok ()

let validate_metadata (meta : session_metadata) : (unit, string) result =
  if String.length meta.document_id = 0 then
    Error "Document ID cannot be empty"
  else if String.length meta.title = 0 then
    Error "Title cannot be empty"
  else if meta.created_at > meta.last_accessed then
    Error "Created time cannot be after last accessed time"
  else
    Ok ()

(* Helper function to convert storage errors to strings *)
let error_to_string = function
  | Database_error msg -> Printf.sprintf "Database error: %s" msg
  | Not_found -> "Not found"
  | Serialization_error msg -> Printf.sprintf "Serialization error: %s" msg
  | Version_mismatch (expected, actual) -> 
      Printf.sprintf "Version mismatch: expected %d, got %d" expected actual

(* Helper functions for List operations *)
let rec take n = function
  | [] -> []
  | x :: xs when n > 0 -> x :: take (n - 1) xs
  | _ -> []