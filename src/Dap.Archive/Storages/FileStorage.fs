[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Archive.Storages.FileStorage

open System
open System.IO
open System.Threading.Tasks
open FSharp.Control.Tasks.V2

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Archive

[<Literal>]
let MetaExtension = ".json"

[<Literal>]
let FramesExtension = ".bytes"

type Param = {
    Root : string
}

type Storage<'extra> when 'extra :> IJson (param') =
    let param : Param = param'
    member _this.Param with get () = param
    interface IStorage<'extra> with
        member _this.OpenFramesStream (runner : IRunner) (relPath : string) =
            let path = Path.Combine (param.Root, relPath, FramesExtension)
            new FileStream (path, FileMode.Open, FileAccess.Read) :> Stream

type Param' = {
    Root : string
    CalcRelPath : string -> string
}

type Storage'<'extra> when 'extra :> IJson (param') =
    let param : Param' = param'
    member _this.Param with get () = param
    interface IStorage'<'extra> with
        member _this.WriteMetaAsync (meta : Meta<'extra>) =
            fun runner -> task {
                let relPath = param.CalcRelPath meta.Key
                let path = Path.Combine (param.Root, relPath)
                checkDirectory runner path "FileStorage"
                use stream = new FileStream (path + MetaExtension, FileMode.Create, FileAccess.Write)
                use writer = new StreamWriter (stream)
                let json = (meta :> IJson).EncodeJson 4
                do! writer.WriteAsync (json)
            }
        member _this.NewFramesStream (runner : IRunner) (key : string) =
            let relPath = param.CalcRelPath key
            let path = Path.Combine (param.Root, relPath)
            checkDirectory runner path "FileStorage"
            new FileStream (path + FramesExtension, FileMode.CreateNew, FileAccess.Write) :> Stream

let calRelPathWithPrefixes (prefixes : int list) (key : string) =
    let len = key.Length
    (prefixes
    |> List.filter (fun p -> p < len)
    |> List.map (fun p -> key.Substring (0, p))
    ) @ [ key ]
    |> List.toArray
    |> Path.Combine

let create'<'extra when 'extra :> IJson> (prefixes : int list) (root : string) =
    let param : Param' =
        {
            Root = root
            CalcRelPath = calRelPathWithPrefixes prefixes
        }
    new Storage'<'extra> (param)