[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Archive.Volume

open System.IO

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote

type Mode =
    | Ready
    | Closed
    | Loaded
    | Reading of BinaryReader

type Param<'frame> = {
    KeepFrames : bool
    ReadFrame : ReadFrame<'frame>
}

type Volume<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame (param', meta') =
    let param : Param<'frame> = param'
    let mutable meta : Meta<'extra> = meta'
    let mutable mode : Mode = Ready
    let mutable frame : 'frame option = None
    let mutable frames : 'frame list = []
    member __.Param with get () = param
    member __.Meta with get () = meta
    member __.Mode with get () = mode
    member __.Frame with get () = frame
    member __.Frames with get () = frames
    member __.Open (runner : IRunner) (stream : Stream) =
        if mode = Closed then
            logError runner "Volume.Open" "Already_Closed" (meta, mode)
            failwith "Volume.Open Failed: Already_Closed"
        if mode <> Ready then
            logError runner "Volume.Open" "Already_Opened" (meta, mode)
            failwith "Volume.Open Failed: Already_Opened"
        if param.KeepFrames then
            //TODO: Load all frames
            mode <- Reading <| new BinaryReader (stream)
        else
            mode <- Reading <| new BinaryReader (stream)
    member __.Close (runner : IRunner) =
        match mode with
        | Ready ->
            logError runner "Volume.Close" "Not_Opened" (meta, mode)
            failwith "Volume.Close Failed: Not_Opened"
        | Closed ->
            logInfo runner "Volume.Close" "Already_Closed" (meta, mode)
            failwith "Volume.Close Failed: Already_Closed"
        | Loaded ->
            mode <- Closed
        | Reading reader ->
            reader.Close()
            mode <- Closed

    member __.ReadFrame (runner : IRunner) : Result<'frame, exn> =
        match mode with
        | Reading reader ->
            param.ReadFrame reader
        | _ ->
            logError runner "Volume.ReadFrame" "Invalid_Mode" (meta, mode)
            failwith <| sprintf "Volume.ReadFrame Failed: Invalid_Mode %A" mode

type Mode' =
    | Ready
    | Closed
    | Writing of BinaryWriter

type Param'<'frame> = {
    KeepFrames : bool
}

type Volume'<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame (param', meta') =
    let param : Param'<'frame> = param'
    let mutable meta : Meta<'extra> = meta'
    let mutable mode : Mode' = Ready
    let mutable frame : 'frame option = None
    let mutable frames : 'frame list = []
    let mutable isDirty : bool = false
    member __.Param with get () = param
    member __.Meta with get () = meta
    member __.Mode with get () = mode
    member __.Frame with get () = frame
    member __.Frames with get () = frames
    member __.Open (runner : IRunner) (stream : Stream) =
        if mode = Closed then
            logError runner "Volume'.Open" "Already_Closed" (meta, mode)
            failwith "Volume'.Open Failed: Already_Closed"
        if mode <> Ready then
            logError runner "Volume'.Open" "Already_Opened" (meta, mode)
            failwith "Volume'.Open Failed: Already_Opened"
        mode <- Writing <| new BinaryWriter (stream)
    member __.Close (runner : IRunner) =
        match mode with
        | Ready ->
            logError runner "Volume'.Close" "Not_Opened" (meta, mode)
            failwith "Volume'.Close Failed: Not_Opened"
        | Closed ->
            logInfo runner "Volume'.Close" "Already_Closed" (meta, mode)
            failwith "Volume'.Close Failed: Already_Closed"
        | Writing writer ->
            writer.Close()
            mode <- Closed

    member __.WriteFrame (runner : IRunner) ((extra, frame') : 'extra * 'frame) : unit =
        match mode with
        | Writing writer ->
            isDirty <- true
            frame <- Some frame'
            frame'.WriteTo writer
            meta <- incLengthOfMeta extra meta
            if param.KeepFrames then
                frames <- frames @ [frame']
        | _ ->
            logError runner "Volume'.WriteFrame" "Invalid_Mode" (meta, mode)
            failwith <| sprintf "Volume'.WriteFrame Failed: Invalid_Mode %A" mode
    member __.Flush (runner : IRunner) : unit =
        if isDirty then
            isDirty <- false
            match mode with
            | Writing writer ->
                writer.Flush()
            | _ ->
                logWarn runner "Volume'.Flush" "Not_Writing" mode

