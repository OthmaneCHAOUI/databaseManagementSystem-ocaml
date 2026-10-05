(* Le type [dbtype] défini les différents types possiblement présents
   en base dans notre cadre.

  Nous n'aurons ici besoin que de deux types primitifs.

  - les entiers représentés ici par TInt
  - les textes représentés ici par TText
 *)
type dbtype =
  | TInt  (* type des entrées entières *)
  | TText (* type des entrées textes   *)
;;

(* Le type [coltype] est le type représentant un champ dans une table
   de notre système.

   Il est composé d'un couple comprenant le type des valeurs présentes
   dans ce champ d'une part et d'un booléen exprimant la possibilité
   (dans le cas où le booléen est à [true]) ou non pour ce champs
   d'adopter la valeur [NULL] *)
type coltype = dbtype * bool ;;

(* Le type [dbvalue] est le type des *VALEURS* présentes en base.

  Nous aurons besoin ici de trois types de valeurs.

  - les valeurs entières munies de leurs valeurs
  - les valeurs textuelles munies de leurs valeurs
  - la valeur null qui pourras être indifféremment considérée de type
   [TInt] et [TText].  *)
type dbvalue =
  | VInt of int     (* valeurs entières   *)
  | VText of string (* valeurs textuelles *)
  | VNull           (* la valeur NULL *)
;;

(* Le schéma d'une table est une liste de couple dont le premier
   élément est le nom du champs et le second est le type du champs de
   type [coltype] *)
type schema = (string*coltype) list ;;


(* Une ligne d'une table est une liste de valeurs.
 *)
type row = dbvalue list ;;

(* Une [table] est la donnée d'un schéma et d'une liste de lignes *)
type table = { cols : schema; rows : row list } ;;

(* Le type [fd] représente le type des dépendances fonctionnelles
   d'une table.

   Il est composé d'un couple (lhs,rhs) dont chacun des deux membres
   est une liste de nom de champs.

   La dépendance (lhs,rhs) représente bien évidement la dépendance lhs -> rhs. 
 *)
type fd = (string list) * (string list) ;;


(* 
 * val check_row_type : row -> schema -> bool
 * @requires   une ligne et les colonnes d'une table
 * @ensures    retourne true si les types des cellules de la ligne sont
 *             les mêmes que celles des colonnes, sinon retourne false
 * @raises     rien
*)
let rec check_row_type row cols =
   match (row, cols) with
   | [], [] -> true
   | [], _ -> false
   | _, [] -> false
   | h :: rest_row, (_, (typedb, accepts_vnull)) :: rest_cols -> 
      (*verifier type VNull*)
      if h = VNull then
         accepts_vnull
      else
         (*verifier autres types*)
         match (h, typedb) with
         | VInt _, TInt -> check_row_type rest_row rest_cols
         | VText _, TText -> check_row_type rest_row rest_cols
         | _ -> false
;;


(* [check_table t] vérifie que la table [t] est valide.
 * val check_table : table -> bool
 * @requires   une table [tbl]
 * @ensures    retourne true si la table est valide, sinon retourne false
 * @raises     rien
 * remarques:  supposons qu'une table est valide si elle verifie : le nombre des 
 *             colonnes = la longueur des lignes, les valeurs et les 
 *             colonnes sont du type correspondants, nullabilité respectée
 *)
let rec check_table tbl = 
   match tbl.rows with
   | [] -> true   (*table vide est valide*)
   | h :: t -> 
      if (List.length h) <> (List.length tbl.cols) then
         false
      else if not (check_row_type h tbl.cols) then
         false
      else
         let rest_of_table = {cols = tbl.cols; rows = t} in
         check_table rest_of_table
;;

(* [insert t r] insère si possible la ligne [r] dans la table [tbl].
 * val insert : table -> row -> table
 * @requires   une table [tbl] et la ligne [r] à insérer dedans
 * @ensures    insère la ligne [r] dans la table [tbl] et retourne la
 *             nouvelle table
 * @raises     lève une exception si : [tbl] est pas valide ou [row] n'a pas
 *             les mêmes types que les colonnes de [tbl] ou la longueur
 *             de [row] est différente de celle des colonnes de [tbl]
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
 * @requires   deux listes des colonnes [cols1] et [cols2]
 * @ensures    retourne true si une colonne de [cols1] a le même nom qu'une
 *             colonne de [cols2], sinon retourne false
 * @raises     rien
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
 * @requires   une liste de lignes [rows1] et une table [tbl2] valide
 * @ensures    retourne le produit cartésien des lignes [rows1] avec
 *             toutes les lignes de [tbl2.rows]
 * @raises     rien
*)
let rec combine_rows rows1 tbl2 =
   match rows1 with
   | [] -> []
   | r1 :: rest ->
         List.map (fun r2 -> r1 @ r2) tbl2.rows @ combine_rows rest tbl2
;;

(* [prod tbl1 tbl2] effectue le produit cartésien des tables [tbl1] et [tbl2]
 * val prod : table -> table -> table
 * @requires   deux tables [tbl1] et [tbl2]
 * @ensures    retourne une table du produit cartésien des deux tables
 * @raises     lève une exception si : au moins une des deux tables n'est pas valide
 *             ou si une colonne d'une table a le même nom que l'autre table
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
 * @requires   une table [tbl] et une liste de noms de champs [fields]
 * @ensures    retourne la liste des indices correspondant aux champs [fields]
 *             dans l'ordre de [tbl.cols]
 * @raises     lève une exception si un champ de [fields] n'existe pas
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
 * @requires   une ligne [row] et une liste d'indices [indices]
 * @ensures    retourne une nouvelle ligne contenant uniquement les valeurs
 *             aux positions données par [indices], dans le même ordre
 * @raises     rien
 *)
let project_row row indices =
  List.map (List.nth row) indices
;;

(*
 * val project_schema : schema -> string list -> schema
 * @requires   une table [tbl] et une liste de noms de champs [fields]
 * @ensures    retourne le nouveau schéma après projection
 * @raises     lève une exception si un champ de [fields] n'existe pas
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

(* [projection tbl fields] effectue la projection suivant la liste de
   champs [fields] de la table [tbl]
 * val projection : table -> string list -> table
 * @requires   une table [tbl] et une liste de noms de champs [fields]
 * @ensures    retourne une table dont le schéma ne contient que les
 *             champs listés dans [fields] (dans l'ordre donné) et les
 *             lignes projetées sur ces champs
 * @raises     lève une exception si : [tbl] n'est pas valide
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

(* [restrict tbl test] effectue la restriction des données présentes
   dans la table [tbl] en accord avec la fonction [test]. On ne garde
   dans le résultat que les lignes pour lesquelles [test] retourne
   [true].
 * val restrict : table -> (row -> bool) -> table
 * @requires   une table [tbl] et une fonction de test [test]
 * @ensures    retourne une table contenant uniquement les lignes de
 *             [tbl] qui satisfont [test]
 * @raises     lève une exception si [tbl] n'est pas valide
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
 * @requires   une table [tbl] valide, deux listes de noms de 
 *             champs [lhs] et [rhs] existant dans [tbl]
 * @ensures    retourne true si la dépendance lhs -> rhs est vérifiée
 *             par toutes les lignes de [tbl], sinon false
 * @raises     rien
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
 * @requires   une liste [lst] d'éléments
 * @ensures    retourne toutes les sous-listes non vides de [lst]
 * @raises     rien
 *)
let rec subsets = function
   | [] -> []
   | x :: rest ->
         let rest_subsets = subsets rest in
         [x] :: List.map (fun s -> x :: s) rest_subsets @ rest_subsets
;;

(* [compute_deps tbl] retourne TOUTES les dépendances fonctionnelles
   trouvées en étudiant les données présentes dans [tbl]
 * val compute_deps : table -> fd list
 * @requires   une table [tbl] valide
 * @ensures    retourne la liste de toutes les dépendances fonctionnelles
 *             lhs -> rhs vérifiées par les données de [tbl]
 * @raises     lève une exception si [tbl] n'est pas valide
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
 * @requires   une table [tbl] valide et une dépendance (lhs, rhs)
 * @ensures    retourne true si la dépendance est élémentaire
 *             (aucun attribut de lhs n'est redondant)
 * @raises     rien
 *)
let is_elementary tbl (lhs, rhs) =
   let rec remove_one = function
      | [] -> []
      | x :: rest -> rest :: List.map (fun l -> x :: l) (remove_one rest)
   in
   let subsets_lhs = remove_one lhs in
   not (List.exists (fun subset -> check_fd tbl subset rhs) subsets_lhs)
;;

(* [compute_elementary_deps tbl] retourne TOUTES les dépendances
   fonctionnelles élémentaires trouvées en étudiant les données
   présentes dans [tbl]
 * val compute_elementary_deps : table -> fd list
 * @requires   une table [tbl] valide
 * @ensures    retourne la liste des dépendances élémentaires vérifiées
 * @raises     lève une exception si [tbl] n'est pas valide
 *)
let compute_elementary_deps tbl =
   if not (check_table tbl) then
      failwith "Error : table not valid"
   else
      let all_deps = compute_deps tbl in
      List.filter (is_elementary tbl) all_deps
;;

(* [normalization_level tbl] retourne le niveau de normalisation de
   [tbl] sous forme d'un entier.
 * val normalization_level : table -> int
 * @requires   une table [tbl] valide
 * @ensures    retourne 1 si la table est en 1NF, 2 si elle est en 2NF,
 *             3 si elle est en 3NF
 * @raises     lève une exception si [tbl] n'est pas valide
 *)
let normalization_level tbl =
   if not (check_table tbl) then
      failwith "Error : table not valid"
   else
      let all_attrs = List.map fst tbl.cols in
      let elem_deps = compute_elementary_deps tbl in
      
      (* 1) Trouver toutes les superclés *)
      let all_subsets = subsets all_attrs in
      let superkeys = List.filter (fun lhs -> check_fd tbl lhs all_attrs) all_subsets in
      
      (* 2) Clés candidates : superclés minimales *)
      let rec is_minimal key =
         not (List.exists (fun k ->
            List.length k < List.length key &&
            List.for_all (fun a -> List.mem a k) key &&
            check_fd tbl k all_attrs
         ) superkeys)
      in
      let candidate_keys = List.filter is_minimal superkeys in
      
      (* 3) Choisir une clé candidate (la plus courte) *)
      let primary_key =
        match List.sort (fun a b -> compare (List.length a) (List.length b)) candidate_keys with
        | [] -> all_attrs   (* cas extrême, ne devrait pas arriver *)
        | k :: _ -> k
      in
      
      (* 4) Attributs premiers = ceux de la clé choisie *)
      let prime_attrs = primary_key in
      
      (* 5) Violation 2NF : lhs sous-ensemble strict de la clé choisie et rhs non premier *)
      let has_partial = List.exists (fun (lhs, rhs) ->
         let lhs_is_proper_subset_of_a_key =
            List.length lhs < List.length primary_key &&
            List.for_all (fun a -> List.mem a primary_key) lhs
         in
         let rhs_has_non_prime = List.exists (fun a -> not (List.mem a prime_attrs)) rhs in
         lhs_is_proper_subset_of_a_key && rhs_has_non_prime
      ) elem_deps in
      
      (* 6) Violation 3NF : lhs non superclé et rhs non premier *)
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