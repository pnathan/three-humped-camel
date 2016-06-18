(*
Licensed unde the Affero Gnu Public License 3.0 or later.

(C) 2016 Paul Nathan


You'll want to fix up a postgres dev database with the following.

CREATE schema saas;
create table saas.triple (id serial, role text, object text, capabilities text);

 *)
open Opium.Std
open Postgresql
open Printf
open Ezjsonm
open Jsonm


type ('a, 'b) either = Left of 'a | Right of 'b

type triple = {
  role: string;
  capability: string;
  obj: string
}

let make_triple role capability obj = { role ; capability; obj }

let json_of_triple { role ; capability ; obj } =
  dict [ "role", (string role) ;
         "capability", (string capability) ;
         "object", (string obj)]

let triple_to_string { role ; capability ; obj } =
  Printf.sprintf "<triple %s ; %s ; %s>" role capability obj


let table_to_json table =
  let row_to_triple row =
    dict [  "role", (string row.(0)) ;
            "capability", (string row.(1)) ;
            "object", (string row.(2)) ] in
  list row_to_triple (Array.to_list table)

type ('a, 'b) complete =  Ok of 'a | Information of 'b

(* type get_res = connection -> result -> ((string array array, string) complete, string) either *)
let get_res conn res =
  match res#status with
  | Empty_query -> Right "Empty query"
  | Command_ok -> Left (Ok "OK")
  | Tuples_ok ->
     printf "polling tuples %d \n" res#ntuples;
     let array = Array.make res#ntuples (Array.make res#nfields "") in
     for tuple = 0 to res#ntuples - 1 do
       printf "getting tuple %d \n" tuple;
       array.(tuple) <- res#get_tuple tuple;
     done;
     Left (Information array)
  | Copy_out ->
     conn#copy_out print_endline;
     Right "Copy out"
  | Copy_in -> Right  "Copy in, not handled!"
  | Bad_response ->
     begin
       let result = Right (sprintf "Bad response: %s" res#error) in
       conn#reset;
       result
     end
  | Nonfatal_error -> Right (sprintf "ERROR: Non fatal error: %s" res#error)
  | Fatal_error -> Right ( sprintf "ERROR: Fatal error: %s" res#error)
  | Copy_both -> Right "Copy in/out, not handled!"
  | Single_tuple -> Right  "Single tuple, not handled!"



let print_res conn res =
  match res#status with
  | Empty_query -> printf "Empty query%!\n"
  | Command_ok -> printf "Command ok [%s]\n" res#cmd_status
  | Tuples_ok ->
      printf "Tuples ok%!\n";
      printf "%i tuples with %i fields\n" res#ntuples res#nfields;
      print_endline (String.concat ";" res#get_fnames_lst);
      for tuple = 0 to res#ntuples - 1 do
        for field = 0 to res#nfields - 1  do
          printf "%s, " (res#getvalue tuple field)
        done;
        print_newline ()
      done
  | Copy_out -> printf "Copy out:\n"; conn#copy_out print_endline
  | Copy_in -> printf "Copy in, not handled!\n"
  | Bad_response -> printf "Bad response: %s\n" res#error; conn#reset
  | Nonfatal_error -> printf "ERROR: Non fatal error: %s\n" res#error
  | Fatal_error -> printf "ERROR: Fatal error: %s\n" res#error
  | Copy_both -> printf "Copy in/out, not handled!\n";
  | Single_tuple -> printf "Single tuple, not handled!\n";
  printf "%!"



let convert_table_to_json table =
  begin
    match table with
    | Left good ->
       begin
         match good with
         | Ok note -> `String note
         (* this is a terrible hack from total ignorance *)
         | Information table -> `String (Ezjsonm.to_string ~minify:false (table_to_json table))
       end
    | Right bad -> `String bad
  end

let get_tuple role capability obj =
  begin
    try
      begin
        let c = new connection ~user:"saas" ~dbname:"dataz" () in
        printf "TRACE: connected\n";
        let res = c#exec ~expect: [Tuples_ok]
                         ~params: [|role; capability; obj|]
                         "SELECT role, capability, object FROM saas.triple WHERE role = $1 AND capability = $2 and object = $3;"
        in
        printf "TRACE: selected\n";
        let result = get_res c res in
        printf "%!TRACE: dumped\n";
        c#finish;
        result
      end
    with Postgresql.Error(error)
         ->
      begin
        printf "ERROR: %s%!" (string_of_error error);
        Right "error"
      end
  end

let get_tuple_method =
  get "/api/v1/capable/:role/:capability/:object"
      begin fun req ->
            let role = param req "role" in
            let capability = param req "capability" in
            let obj = param req "object" in
            let result = get_tuple role capability obj in
            let processed_value = convert_table_to_json result in
            processed_value |> respond'
      end


let all_permissions (): ((string, string array array) complete , string) either =
  begin
    try
      begin
        let c = new connection ~user:"saas" ~dbname:"dataz" () in
        printf "TRACE: connected\n";
        let res = c#exec ~expect: [Tuples_ok] "SELECT role, capability, object FROM saas.triple;" in
        printf "TRACE: selected\n";
        let result = get_res c res in
        printf "%!TRACE: dumped\n";
        c#finish;
        result
      end
    with Postgresql.Error(error)
         ->
      begin
        printf "ERROR: %s%!" (string_of_error error);
        Right "error"
      end
  end

let get_all_permissions =
  get "/api/v1/capable/all"
      begin fun req ->
            let result = all_permissions () in
            let processed_value = convert_table_to_json result in
            processed_value |> respond'
      end

let put_tuple_method =
  put "/api/v1/tuple/:role/:capability/:object"
      begin fun req ->
            let role = param req "role" in
            let capability = param req "capability" in
            let obj = param req "object" in
            try
              begin
                let c = new connection ~user:"saas" ~dbname:"dataz" () in
                printf "TRACE: connected\n";
                let res = c#exec
                              ~expect: [Command_ok]
                              ~params: [|role; capability; obj|]
                              "INSERT INTO  saas.triple (role, capability, object) VALUES ($1, $2, $3)" in
                printf "TRACE: inserted\n";
                get_res c res;
                c#finish;
                `String "ok" |> respond'
              end
            with Postgresql.Error(error)
                 ->
              begin
                printf "ERROR: %s%!" (string_of_error error);
                `String "error" |> respond'
              end
      end

let main () =
  App.empty
  |> get_tuple_method
  |> get_all_permissions
  |> put_tuple_method
  |> App.run_command

let _ =
  printf "INFO: starting up\n";
  main ();
  printf "INFO: quitting\n%!"
