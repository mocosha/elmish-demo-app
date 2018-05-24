module MapComponent

open Elmish
open Fable.Import
open Fable.Helpers.React
open Fable.Helpers.React.Props

type Model = { options: GoogleMap.GoogleMapOptions; mapIsLoaded: bool }

type Msg = 
| ChangeOptions of GoogleMap.GoogleMapOptions
| MapDivLoaded

let mapDivId = "map-div-2"
let defaultMapOptions = { options = { zoom = 9.0m; center = { lat = 44.8107429m; lng = 20.4019244m } }; mapIsLoaded = false }

let private onMapDivLoaded (options) = 
    let element = Util.getHTMLElementByIdOrFail mapDivId
    GoogleMap.createMap element options |> ignore

let init () : Model * Cmd<Msg> =
    defaultMapOptions, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
        | ChangeOptions x -> 
            { model with options = x }, Cmd.none
        | MapDivLoaded ->
            onMapDivLoaded model.options
            { model with mapIsLoaded = true}, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    Browser.console.log "MapComponent.view"
    div [ Id mapDivId
          Ref (fun el -> 
            if not (isNull el) then
                if not model.mapIsLoaded then
                    dispatch MapDivLoaded
          )
          Style [ CSSProp.Height "400px" 
                  CSSProp.Width "300px"] ] 
        [ ]