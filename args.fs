module Args

open System.Data.Common
open Argu
//open System.Data.SqlClient
open Microsoft.Data.SqlClient
open Npgsql
open Lib

type Arguments =
    | [<Mandatory>] ConnectionString of conn:string
    | [<Mandatory>] Directory of dir:string
    | [<Mandatory>] Provider of Provider
    | MakeRecords
    | MakeClasses 
    | Whitelist of tables:string list
    | Blacklist of tables:string list
    | Namespace of nameSpace:string
    | Schema of name:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | ConnectionString _ -> "conn string"
            | Directory _ -> "destiny dir"
            | MakeRecords _ -> "c# records"
            | MakeClasses _ -> "c# classes"
            | Whitelist _ -> "whitelist these tables"
            | Blacklist _ -> "blacklist these tables"
            | Namespace _ -> "namespace"
            | Schema _ -> "schema"
            | Provider _ -> "Provider"
            
let makeFile args =
    let parser = ArgumentParser.Create<Arguments>(programName = "main.exe")
    let results = parser.Parse(args)
    let conn =
        if results.GetResult(Provider) = SQLServer then (new SqlConnection(results.GetResult ConnectionString) :> DbConnection)
        else (new NpgsqlConnection(results.GetResult ConnectionString) :> DbConnection)
    let recordOrClass = if results.Contains MakeClasses then makeCSharpClasses else makeCSharpRecords // Records by default, you can still specify
    let schema = if results.Contains Schema then Some(results.GetResult Schema) else None
    let whitelist = if results.Contains Whitelist then Some(results.GetResult Whitelist)else None
    let blacklist = if results.Contains Blacklist then Some(results.GetResult Blacklist) else None

    let strings = allTables conn schema whitelist blacklist |> recordOrClass

    let file = new System.IO.StreamWriter((results.GetResult Directory))
    file.WriteLine("using System;\nusing System.ComponentModel.DataAnnotations.Schema;\nusing System.ComponentModel.DataAnnotations;")
    if results.Contains(Namespace) then file.WriteLine($"namespace {results.GetResult Namespace};\n")
    for line in strings do file.WriteLine(line)    
    //System.IO.File.WriteAllLines((results.GetResult Directory), strings)
    file.Close()

