[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Archive.Profile

open NodaTime
open NodaTime.Text
open Dap.Prelude
open Dap.Platform
open Dap.Remote

type Profile = {
    CalcVolumeKey : Instant -> string
    VolumeDuration : Duration
}

let perMinute = {
    CalcVolumeKey = InstantFormat.DateHourMinute.Format
    VolumeDuration = Duration.FromMinutes(1L)
}

let perHour = {
    CalcVolumeKey = InstantFormat.DateHour.Format
    VolumeDuration = Duration.FromHours(1)
}

let perDay = {
    CalcVolumeKey = InstantFormat.Date.Format
    VolumeDuration = Duration.FromDays(1)
}

let prefixesDay = [10]
let prefixesMonthDay = [7 ; 10]
let prefixesYearMonthDay = [4 ; 7 ; 10]
let prefixesDayHour = [10 ; 13]
