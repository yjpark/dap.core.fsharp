[<AutoOpen>]
module Dap.Platform.Packs

open System.Threading
open System.Threading.Tasks
open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Platform

module TickerTypes = Dap.Platform.Ticker.Types

(*
 * Generated: <Pack>
 *)
type ITickingPackArgs =
    abstract Ticker : TickerTypes.Args with get

type ITickingPack =
    inherit IPack
    abstract Args : ITickingPackArgs with get
    abstract Ticker : TickerTypes.Agent with get