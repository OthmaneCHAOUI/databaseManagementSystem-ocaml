(* The [dbtype] type defines the different types that may appear in a database
   in our context.

  We only need two primitive types here.

  - integers represented here by TInt
  - text represented here by TText
 *)
type dbtype =
  | TInt  (* type of integer entries *)
  | TText (* type of text entries    *)
;;

(* The [coltype] type is the type representing a field in a table
   in our system.

   It consists of a pair containing the type of the values present
   in that field on one hand and a boolean expressing whether
   (when the boolean is [true]) or not this field may take the value [NULL] *)
type coltype = dbtype * bool ;;

(* The [dbvalue] type is the type of the *VALUES* present in the database.

  We need three kinds of values here.

  - integer values with their values
  - textual values with their values
  - the null value, which may be indifferently considered as type
   [TInt] and [TText].  *)
type dbvalue =
  | VInt of int     (* integer values   *)
  | VText of string (* textual values   *)
  | VNull           (* the NULL value *)
;;

(* A table schema is a list of pairs whose first
   element is the field name and the second is the field type of
   type [coltype] *)
type schema = (string*coltype) list ;;


(* A row of a table is a list of values.
 *)
type row = dbvalue list ;;

(* A [table] is a schema together with a list of rows *)
type table = { cols : schema; rows : row list } ;;

(* The [fd] type represents the type of functional dependencies
   of a table.

   It consists of a pair (lhs, rhs) where each member is a list of
   field names.

   The dependency (lhs, rhs) obviously represents the dependency lhs -> rhs.
 *)
type fd = (string list) * (string list) ;;


(*
 * val check_row_type : row -> schema -> bool
 * @requires   a row and the columns of a table
 * @ensures    returns true if the cell types in the row match those
 *             of the columns, otherwise returns false
 * @raises     nothing
*)
let rec check_row_type row cols =
   match (row, cols) with
   | [], [] -> true
   | [], _ -> false
   | _, [] -> false
   | h :: rest_row, (_, (typedb, accepts_vnull)) :: rest_cols -> 
      (*check VNull type*)
      if h = VNull then
         accepts_vnull
      else
         (*check other types*)
         match (h, typedb) with
         | VInt _, TInt -> check_row_type rest_row rest_cols
         | VText _, TText -> check_row_type rest_row rest_cols
         | _ -> false
;;


(* [check_table t] checks whether table [t] is valid.
 * val check_table : table -> bool
 * @requires   a table [tbl]
 * @ensures    returns true if the table is valid, otherwise false
 * @raises     nothing
 * remarks:  assume a table is valid if it satisfies: the number of
 *           columns equals the length of the rows, values and columns
 *           have matching types, nullability is respected
 *)
let rec check_table tbl = 
   match tbl.rows with
   | [] -> true   (*empty table is valid*)
   | h :: t -> 
      if (List.length h) <> (List.length tbl.cols) then
         false
      else if not (check_row_type h tbl.cols) then
         false
      else
         let rest_of_table = {cols = tbl.cols; rows = t} in
         check_table rest_of_table
;;

(* [insert t r] inserts the row [r] into the table [tbl] if possible.
 * val insert : table -> row -> table
 * @requires   a table [tbl] and the row [r] to insert into it
 * @ensures    inserts row [r] into [tbl] and returns the new table
 * @raises     raises an exception if: [tbl] is not valid or [row] does not
 *             have the same types as the table columns, or the length
 *             of [row] differs from the number of columns in [tbl]
*)
let insert tbl row = 
   if not (check_table tbl) then
      failwith "Error : table not valid\n"
   else if (List.length tbl.cols) <> (List.length row) then
      failwith "Error : different row length\n"
   else if not (check_row_type row tbl.cols) then
      failwith "Error : unexpected row type\n"
   else
      {cols = tbl.cols; rows = tbl.rows@[row]}
;;

(*
 * val common_cols_names : schema -> schema -> bool
 * @requires   two column lists [cols1] and [cols2]
 * @ensures    returns true if a column in [cols1] has the same name as a
 *             column in [cols2], otherwise false
 * @raises     nothing
*)
let rec common_cols_names cols1 cols2 =
   match cols1 with
   | [] -> false
   | (name1, _) :: rest_cols1 ->
         match cols2 with
         | [] -> common_cols_names rest_cols1 cols2
         | (name2, _) :: rest_cols2 ->
            if name1 = name2 then
               true
            else
               common_cols_names cols1 rest_cols2
;;

(*
 * val combine_rows : row list -> table -> row list
 * @requires   a list of rows [rows1] and a valid table [tbl2]
 * @ensures    returns the Cartesian product of rows [rows1] with all
 *             rows in [tbl2.rows]
 * @raises     nothing
*)
let rec combine_rows rows1 tbl2 =
   match rows1 with
   | [] -> []
   | r1 :: rest ->
         List.map (fun r2 -> r1 @ r2) tbl2.rows @ combine_rows rest tbl2
;;

(* [prod tbl1 tbl2] computes the Cartesian product of tables [tbl1] and [tbl2]
 * val prod : table -> table -> table
 * @requires   two tables [tbl1] and [tbl2]
 * @ensures    returns a table containing the Cartesian product of the two tables
 * @raises     raises an exception if: at least one of the two tables is invalid
 *             or if a column in one table has the same name as a column in the other
*)                   
let prod tbl1 tbl2 =
   if not (check_table tbl1) || not (check_table tbl2) then
      failwith "Error : tables not valid\n"
   else if common_cols_names tbl1.cols tbl2.cols then
       failwith "Error : common column names\n"
   else
      let res_cols = tbl1.cols@tbl2.cols in
      let res_rows = combine_rows tbl1.rows tbl2 in
      {cols = res_cols; rows = res_rows}
;;

(*
 * val get_indices : schema -> string list -> int list
 * @requires   a table [tbl] and a list of field names [fields]
 * @ensures    returns the list of indices corresponding to the fields [fields]
 *             in the order of [tbl.cols]
 * @raises     raises an exception if a field in [fields] does not exist
*)
let get_indices cols fields =
   let rec find_index name = function
      | [] -> failwith ("Field not found: " ^ name)
      | (n, _) :: rest ->
         if n = name then
            0
         else
            1 + find_index name rest
   in
   List.map (fun field -> find_index field cols) fields
;;

(*
 * val project_row : row -> int list -> row
 * @requires   a row [row] and a list of indices [indices]
 * @ensures    returns a new row containing only the values at the positions
 *             specified by [indices], in the same order
 * @raises     nothing
 *)
let project_row row indices =
  List.map (List.nth row) indices
;;

(*
 * val project_schema : schema -> string list -> schema
 * @requires   a table [tbl] and a list of field names [fields]
 * @ensures    returns the new schema after projection
 * @raises     raises an exception if a field in [fields] does not exist
 *)
let project_schema cols fields =
   let rec find_col name = function
      | [] -> failwith ("Field not found: " ^ name)
      | (n, t) :: rest ->
         if n = name then
            (n, t)
         else
            find_col name rest
   in
   List.map (fun field -> find_col field cols) fields
;;

(* [projection tbl fields] performs the projection on the list of
   fields [fields] of table [tbl]
 * val projection : table -> string list -> table
 * @requires   a table [tbl] and a list of field names [fields]
 * @ensures    returns a table whose schema contains only the fields listed in
 *             [fields] (in the given order) and whose rows are projected on those fields
 * @raises     raises an exception if: [tbl] is not valid
 *)
let projection tbl fields =
   if not (check_table tbl) then
      failwith "Error : table not valid"
   else
      let indices = get_indices tbl.cols fields in
      let new_cols = project_schema tbl.cols fields in
      let new_rows = List.map (fun row -> project_row row indices) tbl.rows in
      { cols = new_cols; rows = new_rows }
;;

(* [restrict tbl test] restricts the data present in table [tbl]
   according to the function [test]. Only rows for which [test]
   returns [true] are kept in the result.
 * val restrict : table -> (row -> bool) -> table
 * @requires   a table [tbl] and a test function [test]
 * @ensures    returns a table containing only the rows of [tbl] that satisfy [test]
 * @raises     raises an exception if [tbl] is not valid
 *)
let restrict tbl test =
   if not (check_table tbl) then
      failwith "Error : table not valid"
   else
      let new_rows = List.filter test tbl.rows in
      { cols = tbl.cols; rows = new_rows }
;;  

(*
 * val check_fd : table -> string list -> string list -> bool
 * @requires   a valid table [tbl], two lists of field names [lhs] and [rhs]
 *             that exist in [tbl]
 * @ensures    returns true if the dependency lhs -> rhs holds for all rows in [tbl],
 *             otherwise false
 * @raises     nothing
 *)
let check_fd tbl lhs rhs =
   let indices_lhs = get_indices tbl.cols lhs in
   let indices_rhs = get_indices tbl.cols rhs in
   let rec aux = function
      | [] -> true
      | row1 :: rest ->
         let key1 = project_row row1 indices_lhs in
         let val1 = project_row row1 indices_rhs in
         let conflict = List.exists (fun row2 -> key1 = project_row row2 indices_lhs && val1 <> project_row row2 indices_rhs) rest in
         if conflict then false else aux rest
   in
   aux tbl.rows
;;

(*
 * val subsets : 'a list -> 'a list list
 * @requires   a list [lst] of elements
 * @ensures    returns all non-empty sublists of [lst]
 * @raises     nothing
 *)
let rec subsets = function
   | [] -> []
   | x :: rest ->
         let rest_subsets = subsets rest in
         [x] :: List.map (fun s -> x :: s) rest_subsets @ rest_subsets
;;

(* [compute_deps tbl] returns ALL functional dependencies
   found by examining the data present in [tbl]
 * val compute_deps : table -> fd list
 * @requires   a valid table [tbl]
 * @ensures    returns the list of all functional dependencies
 *             lhs -> rhs satisfied by the data in [tbl]
 * @raises     raises an exception if [tbl] is not valid
 *)
let compute_deps tbl =
   if not (check_table tbl) then
      failwith "Error : table not valid"
   else
      let all_attrs = List.map fst tbl.cols in
      let all_subsets = subsets all_attrs in
      let rec find_fds = function
         | [] -> []
         | lhs :: rest_lhs ->
            let possible_rhs = List.filter (fun rhs ->
               rhs <> [] && List.for_all (fun x -> not (List.mem x lhs)) rhs
            ) all_subsets in
            let valid_rhs = List.filter (fun rhs -> check_fd tbl lhs rhs) possible_rhs in
            let fds = List.map (fun rhs -> (lhs, rhs)) valid_rhs in
            fds @ find_fds rest_lhs
      in
      find_fds all_subsets
;;

(*
 * val is_elementary : table -> fd -> bool
 * @requires   a valid table [tbl] and a dependency (lhs, rhs)
 * @ensures    returns true if the dependency is elementary
 *             (no attribute in lhs is redundant)
 * @raises     nothing
 *)
let is_elementary tbl (lhs, rhs) =
   let rec remove_one = function
      | [] -> []
      | x :: rest -> rest :: List.map (fun l -> x :: l) (remove_one rest)
   in
   let subsets_lhs = remove_one lhs in
   not (List.exists (fun subset -> check_fd tbl subset rhs) subsets_lhs)
;;

(* [compute_elementary_deps tbl] returns ALL elementary functional
   dependencies found by examining the data present in [tbl]
 * val compute_elementary_deps : table -> fd list
 * @requires   a valid table [tbl]
 * @ensures    returns the list of elementary dependencies satisfied
 * @raises     raises an exception if [tbl] is not valid
 *)
let compute_elementary_deps tbl =
   if not (check_table tbl) then
      failwith "Error : table not valid"
   else
      let all_deps = compute_deps tbl in
      List.filter (is_elementary tbl) all_deps
;;

(* [normalization_level tbl] returns the normalization level of
   [tbl] as an integer.
 * val normalization_level : table -> int
 * @requires   a valid table [tbl]
 * @ensures    returns 1 if the table is in 1NF, 2 if it is in 2NF,
 *             3 if it is in 3NF
 * @raises     raises an exception if [tbl] is not valid
 *)
let normalization_level tbl =
   if not (check_table tbl) then
      failwith "Error : table not valid"
   else
      let all_attrs = List.map fst tbl.cols in
      let elem_deps = compute_elementary_deps tbl in
      
      (* 1) Find all superkeys *)
      let all_subsets = subsets all_attrs in
      let superkeys = List.filter (fun lhs -> check_fd tbl lhs all_attrs) all_subsets in
      
      (* 2) Candidate keys: minimal superkeys *)
      let rec is_minimal key =
         not (List.exists (fun k ->
            List.length k < List.length key &&
            List.for_all (fun a -> List.mem a k) key &&
            check_fd tbl k all_attrs
         ) superkeys)
      in
      let candidate_keys = List.filter is_minimal superkeys in
      
      (* 3) Choose a candidate key (the shortest one) *)
      let primary_key =
        match List.sort (fun a b -> compare (List.length a) (List.length b)) candidate_keys with
        | [] -> all_attrs   (* extreme case, should not happen *)
        | k :: _ -> k
      in
      
      (* 4) Prime attributes = those in the chosen key *)
      let prime_attrs = primary_key in
      
      (* 5) 2NF violation: lhs is a strict subset of the chosen key and rhs is not prime *)
      let has_partial = List.exists (fun (lhs, rhs) ->
         let lhs_is_proper_subset_of_a_key =
            List.length lhs < List.length primary_key &&
            List.for_all (fun a -> List.mem a primary_key) lhs
         in
         let rhs_has_non_prime = List.exists (fun a -> not (List.mem a prime_attrs)) rhs in
         lhs_is_proper_subset_of_a_key && rhs_has_non_prime
      ) elem_deps in
      
      (* 6) 3NF violation: lhs is not a superkey and rhs is not prime *)
      let has_transitive = List.exists (fun (lhs, rhs) ->
         let lhs_is_superkey = List.exists (fun key ->
            List.for_all (fun a -> List.mem a key) lhs && check_fd tbl lhs all_attrs
         ) superkeys in
         let rhs_has_non_prime = List.exists (fun a -> not (List.mem a prime_attrs)) rhs in
            (not lhs_is_superkey) && rhs_has_non_prime
      ) elem_deps in
      
      if has_partial then 1
      else if has_transitive then 2
      else 3
;;

(* =================== tests =================== *)

let run_tests () =
   Printf.printf "Running tests...\n\n";
   
   (* Test check_table *)
   let cols1 = [("id", (TInt, false)); ("nom", (TText, true))] in
   let tbl1 = { cols = cols1; rows = [[VInt 1; VText "Alice"]] } in
   if check_table tbl1 then
      Printf.printf "OK check_table: basic table valid\n"
   else
      Printf.printf "FAIL check_table: basic table should be valid\n";
   
   let tbl2 = { cols = cols1; rows = [[VInt 1; VText "Alice"]; [VInt 2; VNull]] } in
   if check_table tbl2 then
      Printf.printf "OK check_table: with NULL (nullable) valid\n"
   else
      Printf.printf "FAIL check_table: table with NULL should be valid\n";
   
   let tbl3 = { cols = cols1; rows = [[VNull; VText "Alice"]] } in
   if not (check_table tbl3) then
      Printf.printf "OK check_table: NULL on non-nullable column invalid\n"
   else
      Printf.printf "FAIL check_table: NULL on non-nullable column should be invalid\n";
   
   (* Test insert *)
   let tbl4 = { cols = cols1; rows = [] } in
   let tbl5 = insert tbl4 [VInt 1; VText "Alice"] in
   if List.length tbl5.rows = 1 then
      Printf.printf "\nOK insert: adds row correctly\n"
   else
      Printf.printf "FAIL insert: expected 1 row, got %d\n" (List.length tbl5.rows);
   
   let tbl6 = { cols = cols1; rows = [] } in
   try
     let _ = insert tbl6 [VInt 1] in
     Printf.printf "FAIL insert: should fail with wrong length\n"
   with _ -> Printf.printf "OK insert: rejects wrong length\n";
   
   (* Test prod *)
   let tbl_a = { cols = [("a", (TInt, false))]; rows = [[VInt 1]; [VInt 2]] } in
   let tbl_b = { cols = [("b", (TInt, false))]; rows = [[VInt 10]; [VInt 20]] } in
   let tbl_prod = prod tbl_a tbl_b in
   if List.length tbl_prod.rows = 4 then
      Printf.printf "\nOK prod: cartesian product correct\n"
   else
      Printf.printf "FAIL prod: expected 4 rows, got %d\n" (List.length tbl_prod.rows);
   
   let tbl_c = { cols = [("a", (TInt, false))]; rows = [[VInt 1]] } in
   let tbl_d = { cols = [("a", (TInt, false))]; rows = [[VInt 2]] } in
   try
     let _ = prod tbl_c tbl_d in
     Printf.printf "FAIL prod: should fail with duplicate column names\n"
   with _ -> Printf.printf "OK prod: rejects duplicate column names\n";
   
   (* test projection *)
   let tbl_proj = projection tbl1 ["id"] in
   if List.length tbl_proj.cols = 1 && tbl_proj.rows = [[VInt 1]] then
      Printf.printf "\nOK projection: works correctly\n"
   else
      Printf.printf "FAIL projection: unexpected result\n";
   
   let tbl_proj2 = projection tbl1 ["nom"; "id"] in
   if List.length tbl_proj2.cols = 2 && tbl_proj2.rows = [[VText "Alice"; VInt 1]] then
      Printf.printf "OK projection: multiple fields in order\n"
   else
      Printf.printf "FAIL projection: field order problem\n";
   
   let tbl7 = { cols = cols1; rows = [] } in
   try
     let _ = projection tbl7 ["age"] in
     Printf.printf "FAIL projection: should fail with non-existent field\n"
   with _ -> Printf.printf "OK projection: rejects non-existent field\n";
   
   (* test restrict *)
   let tbl_rest = restrict tbl1 (fun row -> row = [VInt 1; VText "Alice"]) in
   if List.length tbl_rest.rows = 1 then
      Printf.printf "\nOK restrict: filters correctly\n"
   else
      Printf.printf "FAIL restrict: expected 1 row, got %d\n" (List.length tbl_rest.rows);
   
   let tbl_rest2 = restrict tbl1 (fun _ -> false) in
   if List.length tbl_rest2.rows = 0 then
      Printf.printf "OK restrict: returns empty when all filtered\n"
   else
      Printf.printf "FAIL restrict: expected 0 rows, got %d\n" (List.length tbl_rest2.rows);
   
   (* test compute_deps *)
   let tbl_deps = { cols = [("a", (TInt, false)); ("b", (TInt, false)); ("c", (TInt, false))];
                    rows = [[VInt 1; VInt 2; VInt 3];
                            [VInt 1; VInt 2; VInt 3];
                            [VInt 1; VInt 3; VInt 4]] } in
   let deps = compute_deps tbl_deps in
   let has_fd = List.exists (fun (lhs, rhs) -> lhs = ["a"; "b"] && rhs = ["c"]) deps in
   if has_fd then
      Printf.printf "\nOK compute_deps: finds functional dependencies\n"
   else
      Printf.printf "FAIL compute_deps: should find a,b -> c\n";
    
   (* test compute_elementary_deps *)
   let elem_deps = compute_elementary_deps tbl_deps in
   let is_elem = List.for_all (fun (lhs, rhs) -> List.length lhs <= 2) elem_deps in
   if is_elem then
      Printf.printf "\nOK compute_elementary_deps: returns elementary dependencies\n"
   else
      Printf.printf "FAIL compute_elementary_deps: should have no redundant attributes\n";
   
   (* test normalization_level *)
   (* test 1NF  *)
   let tbl_non_2nf = 
   { cols = [("id_commande", (TInt, false)); 
               ("id_produit", (TInt, false)); 
               ("nom_produit", (TText, false))];
      rows = [[VInt 1; VInt 101; VText "Ordinateur"];
               [VInt 1; VInt 102; VText "Souris"];
               [VInt 2; VInt 101; VText "Ordinateur"]] } in
   let level1 = normalization_level tbl_non_2nf in
   if level1 = 1 then
      Printf.printf "\nOK normalization_level: partial dependency -> 1NF (got %d)\n" level1
   else
      Printf.printf "FAIL normalization_level: partial dependency should be 1NF, got %d\n" level1;

   (* test 2NF (transitive dependency) *)
   let tbl_non_3nf = 
   { cols = [("id_employe", (TInt, false));
               ("id_departement", (TInt, false));
               ("nom_departement", (TText, false))];
      rows = [[VInt 1; VInt 10; VText "Informatique"];
               [VInt 2; VInt 10; VText "Informatique"];
               [VInt 3; VInt 20; VText "Marketing"]] } in
   let level2 = normalization_level tbl_non_3nf in
   if level2 = 2 then
      Printf.printf "OK normalization_level: transitive dependency -> 2NF (got %d)\n" level2
   else
      Printf.printf "FAIL normalization_level: transitive dependency should be 2NF, got %d\n" level2;

   (* test 3NF *)
   let tbl_3nf = { cols = [("id", (TInt, false)); ("nom", (TText, true))]; rows = [[VInt 1; VText "Alice"]; [VInt 2; VText "Bob"]] } in
   let level3 = normalization_level tbl_3nf in
   if level3 = 3 then
      Printf.printf "OK normalization_level: normalized -> 3NF (got %d)\n" level3
   else
      Printf.printf "FAIL normalization_level: normalized table should be 3NF, got %d\n" level3;
  
   Printf.printf "\nAll tests completed!\n"
;;

let () = run_tests ()