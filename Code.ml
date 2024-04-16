(* Type Definitions *)

(* Enum type for supported SQL column types *)
type t_tablecolumnType = INT | CHAR of int;;

(* Record type for column definition in a table *)
type t_tableColDef = {defColName : string; columnType : t_tablecolumnType};;

(* Variant type for storing column data values *)
type t_tableColDataVal = Int of int | Char of string;;

(* Record type for column data, associating column names with their values *)
type t_tableColData = {dateColName : string; value : t_tableColDataVal};;

(* Record type for representing a row in a table as a list of column data *)
type t_tableRowData = {data : t_tableColData list};;

(* Record type for a table, including its name, column definitions, and rows *)
type t_table = {tableName : string; coldeflist : t_tableColDef list; rowdatalist : t_tableRowData list};;

(* Record type for representing the entire database, containing a list of tables *)
type t_dbms = { dbmsname : string; tableList : t_table list};;

(* Enum type for relational operators used in comparisons *)
type t_relationship = OpEq | OpNe ;;

(* Enum type for supported aggregation operations *)
type t_aggeragations = Avg | Sum;;

(* Record type for predicates used in query conditions *)
type t_predicate = { preddata : t_tableColData; predoper : t_relationship };;


(* Helper Functions *)

(* Recursive function to replace a table in a list with an updated version *)
let rec table_Substitution tableList table accumalator = match tableList with
  | [] -> accumalator
  | t::ts -> if (t.tableName = table.tableName) then
               table_Substitution ts table (table :: accumalator)
             else
               table_Substitution ts table (t :: accumalator);;

(* Recursive helper function to find a table by name *)
let rec get_TableAux tableList tableName = match tableList with
  | [] -> None
  | t::ts -> if (t.tableName = tableName) then
               Some t
             else
               get_TableAux ts tableName;;
               
(* Retrieves a table from the list or fails with an error message if not found *)
let rec get_Table tableList tableName = 
  let t = get_TableAux tableList tableName in
  match t with 
  | None -> failwith ("ERROR: TABLE IS NOT FOUND" ^ tableName)
  | Some table -> table;;


(* Database Operations *)

(* Function to create a new database with a specified name *)
let create_Database databasename = 
  { dbmsname = databasename; tableList = [] };;

(* Function to add a new, empty table to the database *)
let create_Table dbms tableName =
  {
    dbms with tableList = dbms.tableList @ [{tableName = tableName; coldeflist = []; rowdatalist = [] }]
  };;

(* Function to remove a table from the database *)
let rec drop_Table_Aux tableList tableName accumalator = match tableList with
  | [] -> accumalator
  | x::xs -> if (x.tableName = tableName) then 
                accumalator @ xs
              else
                x::(drop_Table_Aux xs tableName accumalator);;

let drop_Table dbms tableName = 
       let newtableList = drop_Table_Aux dbms.tableList tableName []
       in 
       {dbms with tableList = newtableList};;


(* Column Management *)

(* Function to add a new column to a table *)
let add_Column table coldef = 
  {table with coldeflist = table.coldeflist @ [coldef]};;

(* Function to add a new column to a specified table within the database *)
let add_ColumnToTable dbms tableName colDef = 
{
    dbms with tableList = (table_Substitution dbms.tableList (add_Column (get_Table dbms.tableList tableName) colDef) []) 
};;
