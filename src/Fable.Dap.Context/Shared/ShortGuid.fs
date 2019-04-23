[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.ShortGuid

open System

// Copied from Giraffe Common.fs

// ---------------------------
// Short GUIDs and IDs
// ---------------------------

/// **Description**
///
/// Short GUIDs are a shorter, URL-friendlier version
/// of the traditional `System.Guid` type.
///
/// Short GUIDs are always 22 characters long, which let's
/// one save a total of 10 characters in comparison to using
/// a normal `System.Guid` as identifier.
///
/// Additionally a Short GUID is by default a URL encoded
/// string which doesn't need extra character replacing
/// before using of it in a URL query parameter.
///
/// All Short GUID strings map directly to a `System.Guid`
/// objet and the `ShortGuid` module can be used to convert
/// a `System.Guid` into a short GUID `string` and vice versa.
///
/// For more information please check:
/// https://madskristensen.net/blog/A-shorter-and-URL-friendly-GUID
///

/// **Description**
///
/// Converts a `System.Guid` into a 22 character long
/// short GUID string.
///
/// **Parameters**
///
/// `guid`: The `System.Guid` to be converted into a short GUID.
///
/// **Output**
///
/// Returns a 22 character long URL encoded short GUID string.
///
let fromGuid (guid : Guid) =
    guid.ToByteArray()
    |> Convert.ToBase64String
    |> (fun str ->
        str.Replace("/", "_")
            .Replace("+", "-")
            .Substring(0, 22))

/// **Description**
///
/// Converts a 22 character short GUID string into the matching `System.Guid`.
///
/// **Parameters**
///
/// `shortGuid`: The short GUID string to be converted into a `System.Guid`.
///
/// **Output**
///
/// Returns a `System.Guid` object.
///
let toGuid (shortGuid : string) =
    shortGuid.Replace("_", "/")
                .Replace("-", "+")
    |> (fun str -> str + "==")
    |> Convert.FromBase64String
    |> Guid
