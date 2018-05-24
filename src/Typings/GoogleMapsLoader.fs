// ts2fable 0.6.1
module rec GoogleMapsLoader
open System
open Fable.Core
open Fable.Import.JS

let [<Import("*","google-maps")>] googleMapsLoader: GoogleMapsLoader.IExports = jsNative

module GoogleMapsLoader =

    type [<AllowNullLiteral>] IExports =
        abstract KEY: string
        abstract URL: string
        abstract LIBRARIES: Array<string>
        abstract CLIENT: string
        abstract CHANNEL: string
        abstract LANGUAGE: string
        abstract REGION: string
        abstract VERSION: string
        abstract WINDOW_CALLBACK_NAME: string
        abstract release: callBack: Function -> unit
        abstract onLoad: ?callBack: CallBack -> unit
        abstract load: ?callBack: CallBack -> unit
        abstract isLoaded: unit -> bool
        abstract createLoader: unit -> unit
        abstract createUrl: unit -> string
        abstract makeMock: unit -> unit

    type [<AllowNullLiteral>] google =
        abstract maps: obj with get, set

    type [<AllowNullLiteral>] CallBack =
        [<Emit "$0($1...)">] abstract Invoke: google: google -> unit
