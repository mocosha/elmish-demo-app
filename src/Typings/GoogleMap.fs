module GoogleMap
    open System
    open Fable.Core
    open Fable.Import
    open Fable.PowerPack

    type GoogleLatLng = { 
        lat: unit -> Decimal
        lng: unit -> Decimal
    }

    type GoogleLatLngOption = {
        lat: Decimal
        lng: Decimal
    }

    type GoogleMapOptions = {
        zoom: Decimal
        center: GoogleLatLngOption
    }

    type IGoogleMaps = {
        getCenter: unit ->  GoogleLatLng
    }

    [<Emit("new google.maps.Map($0, $1)")>]
    let private _createMap (element: Browser.HTMLElement) (options: GoogleMapOptions) : IGoogleMaps = jsNative

    [<Emit("$0.addListener($1, $2)")>]
    let addMapListener (map: IGoogleMaps) (event: string) (callback: unit -> unit) : IGoogleMaps = jsNative

    [<Emit("$0.addListener('idle', $1)")>]
    let addMapListener_idle (map: IGoogleMaps) (callback: unit -> unit) : IGoogleMaps = jsNative

    [<Emit("$0.addListener('drag', $1)")>]
    let addMapListener_drag (map: IGoogleMaps) (callback: unit -> unit) : IGoogleMaps = jsNative
    
    [<Emit("$0.getCenter()")>]
    let getMapCenter (map: IGoogleMaps): GoogleLatLng = jsNative
    
    [<Emit("new google.maps.LatLng($0, $1)")>]
    let createMapLatLng (lat: Decimal) (lng: Decimal): GoogleLatLng = jsNative

    let createMap (element: Browser.HTMLElement) (options: GoogleMapOptions)  = 
        let map = _createMap element options
        let mutable loaded = false
        addMapListener_idle map (fun _ -> loaded <- true) |> ignore

        promise {
            while not loaded do
                do! Promise.sleep 100
            return map
        }