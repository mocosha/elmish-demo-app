module Util

open Fable.Import

let toListOf<'a> (nodeList:Browser.NodeListOf<'a>) =
    let length = nodeList.length - 1.0
    [0.0 .. length]
    |> List.map (nodeList.item)

let getHTMLElementByIdOrFail (id: string) =
    let el = Browser.document.getElementById id
    if isNull el then
        failwith ( sprintf "Element with id: %s not found" id )
    else 
        el