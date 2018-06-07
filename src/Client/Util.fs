module Util

open Fable.Import

let getHTMLElementByIdOrFail (id: string) =
    let el = Browser.document.getElementById id
    if isNull el then
        failwith ( sprintf "Element with id: %s not found" id )
    else 
        el