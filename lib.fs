module Lib

open System
open System.Data.Common

type Provider =
    | SQLServer
    | Postgres

type ColumnInfo = {Name:string; IsNullable:bool; DataType:string; MaxChars:int option; Precision:int option; Scale:int option}

let TypeConverter =
    [| 
        ( "bigint", "long" );
        ( "binary varying", "byte[]" );
        ( "binary", "byte[]" );
        ( "bit", "bool" );
        ( "boolean", "bool");
        ( "bytea", "byte[]")
        ( "char varying", "string" );
        ( "char varying(max)", "string" );
        ( "char", "string" );
        ( "character varying", "string" );
        ( "character varying(max)", "string" );
        ( "character", "string" );
        ( "date", "DateTime" );
        ( "datetime", "DateTime" );
        ( "datetime2", "DateTime" );
        ( "datetimeoffset", "DateTimeOffset" );
        ( "dec", "decimal" );
        ( "decimal", "decimal" );
        ( "double precision", "double" );
        ( "float", "double" );
        ( "image", "byte[]" );
        ( "int", "int" );
        ( "integer", "int" );
        ( "money", "decimal" );
        ( "national char varying", "string" );
        ( "national char varying(max)","string" );
        ( "national character varying", "string" );
        ( "national character varying(max)","string" );
        ( "national character", "string" );
        ( "nchar", "string" );
        ( "ntext", "string" );
        ( "numeric", "decimal" );
        ( "nvarchar", "string" );
        ( "nvarchar(max)","string" );
        ( "real", "double" );
        //( "rowversion", _rowversion );
        ( "smalldatetime", "DateTime" );
        ( "smallint", "short" );
        ( "smallmoney", "decimal" );
        //( "sql_variant", _sqlVariant );
        ( "text", "string" );
        ( "time", "TimeSpan" );
        ( "time without time zone", "TimeSpan")
        ( "timestamp without time zone", "DateTime")
        //( "timestamp", _rowversion );
        ( "tinyint", "byte" );
        ( "uniqueidentifier", "Guid" );
        ( "uuid", "Guid");
        ( "varbinary", "byte[]" );
        ( "varbinary(max)", "byte[]" );
        ( "varchar", "string" );
        ( "varchar(max)", "string" );
        ( "xml", "string" );
    |] 
    |> Map.ofArray

let capitalize (s:string) = String.mapi (fun i c -> match i with | 0 -> (Char.ToUpper c) | _ -> c) s

let addAnnotations (col:ColumnInfo) =
    let columnAttribute = 
        if col.DataType = "decimal" then $"""{"\t"}[Column("{col.Name}", TypeName = "decimal({col.Precision.Value},{col.Scale.Value})")]{"\n"}"""
        else  $"""{"\t"}[Column("{col.Name}")]{"\n"}"""
    let stringLengthAttribute = $"""{match col.MaxChars with | None -> "" | Some (maxChars) -> $"\t[StringLength({maxChars})]\n"}"""
    $"{columnAttribute}{stringLengthAttribute}"

let rec getClassProperties (columns:ColumnInfo list) =
    match columns with
    | [] -> ""
    | col::tail -> 
        try
            $"""{addAnnotations col}{"\t"}public {TypeConverter.[col.DataType]}{if col.IsNullable then "?" else ""} {capitalize (col.Name)} {{ get; set; }}{"\n"}{getClassProperties tail}"""
        with ex ->
            $"""{"\t//"}Unsupported type: {col.DataType}{"\n"}{"\t//"}public {col.DataType}{if col.IsNullable then "?" else ""} {capitalize (col.Name)} {{ get; set; }}{"\n"}{getClassProperties tail}"""

let makeCSharpClasses (tables: (string * ColumnInfo list) array) =
    [|for table in tables do yield $"""[Table("{fst table}")]{"\n"}public class {fst table}{"\n"}{{{"\n"}{getClassProperties (snd table)}}}{"\n"}"""|]

let makeCSharpRecords (tables: (string * ColumnInfo list) array) =
    [|for table in tables do yield $"""[Table("{fst table}")]{"\n"}public record {fst table}{"\n"}{{{"\n"}{getClassProperties (snd table)}}};{"\n"}"""|]


///// conn stuff /////

let getTableInfo (conn:DbConnection) (tableName:string) =
    (tableName, 
        [
        for row in conn.GetSchema("Columns", [|null;null;tableName|]).Rows do
        yield      
            {
            Name = row.["COLUMN_NAME"].ToString(); 
            IsNullable = if row.["IS_NULLABLE"].ToString() = "YES" then true else false; 
            DataType = row.["DATA_TYPE"].ToString();
            MaxChars = if row.IsNull "CHARACTER_MAXIMUM_LENGTH" then None else Some(int (row.["CHARACTER_MAXIMUM_LENGTH"].ToString()))
            Precision = if row.IsNull "NUMERIC_PRECISION" then None else Some(int (row.["NUMERIC_PRECISION"].ToString()))
            Scale = if row.IsNull "NUMERIC_SCALE" then None else Some(int (row.["NUMERIC_SCALE"].ToString()))
            }
        ]
    )

let allTables (conn:DbConnection) (schema:string option) (whitelist: string list option) (blacklist:string list option) =
    let getName (row:Data.DataRow) = row.["TABLE_NAME"].ToString()
    conn.Open()
    try
        let mutable tableNames =
            match schema with
            | None -> [|for row in conn.GetSchema("Tables").Rows do if not ((getName row).Contains "_Archive") then yield getName row|]
            | Some(schema) -> [|for row in conn.GetSchema("Tables").Rows do if (getName row) = schema && not ((getName row).Contains "_Archive") then yield row.["TABLE_NAME"].ToString()|]

        if whitelist.IsSome then tableNames <- [|for table in tableNames do if (List.contains table whitelist.Value) then yield table|]
        if blacklist.IsSome then tableNames <- [|for table in tableNames do if not (List.contains table blacklist.Value) then yield table|]

        Array.map (getTableInfo conn) tableNames
    finally
        conn.Close()

// let whitelist (conn:DbConnection) (whitelist:string list) (schema:string option) =
//     conn.Open()
//     try
//         match schema with
//         | None ->
//             [|for row in conn.GetSchema("Tables").Rows do if (List.contains (row.["TABLE_NAME"].ToString()) whitelist) && not (row.["TABLE_NAMES"].ToString().Contains "_Archive") then yield row.["TABLE_NAME"].ToString()|]
//         | Some(schema) ->
//             [|for row in conn.GetSchema("Tables").Rows do if row.["TABLE_SCHEMA"].ToString() = schema && (List.contains (row.["TABLE_NAME"].ToString()) whitelist) && not (row.["TABLE_NAMES"].ToString().Contains "_Archive") then yield row.["TABLE_NAME"].ToString()|]
//         |> Array.map (getTableInfo conn)
//     finally
//         conn.Close()

// let blacklist (conn:DbConnection) (blacklist:string list) (schema:string option) =
//     conn.Open()
//     try
//         match schema with
//         | None ->
//             [|for row in conn.GetSchema("Tables").Rows do if not (List.contains (row.["TABLE_NAME"].ToString()) blacklist) then yield row.["TABLE_NAME"].ToString()|]
//         | Some(schema) ->
//             [|for row in conn.GetSchema("Tables").Rows do if row.["TABLE_SCHEMA"].ToString() = schema && not (List.contains (row.["TABLE_NAME"].ToString()) blacklist) then yield row.["TABLE_NAME"].ToString()|]
//         |> Array.map (getTableInfo conn)
//     finally
//         conn.Close()

