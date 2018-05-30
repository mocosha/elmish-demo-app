module MapComponent

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props

type Model = { options: GoogleMap.GoogleMapOptions; mapIsLoaded: bool }

type Msg = 
| ChangeOptions of GoogleMap.GoogleMapOptions
| MapDivLoaded of string
| MapDivUnloaded

let mapDivId () = "map-div-" + System.DateTime.Now.Millisecond.ToString()
let defaultMapOptions = { options = { zoom = 9.0m; center = { lat = 44.8107429m; lng = 20.4019244m } }; mapIsLoaded = false }

let private onMapDivLoaded (mapDivId) (options) = 
    let element = Util.getHTMLElementByIdOrFail mapDivId
    GoogleMap.createMap element options |> ignore

let init () : Model * Cmd<Msg> =
    defaultMapOptions, Cmd.none

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
        | ChangeOptions x -> 
            { model with options = x }, Cmd.none
        | MapDivLoaded mapDivId ->
            onMapDivLoaded mapDivId model.options
            { model with mapIsLoaded = true}, Cmd.none
        | MapDivUnloaded ->
            { model with mapIsLoaded = false}, Cmd.none
 
let view (model : Model) (dispatch : Msg -> unit) =
    div [ Ref (fun el -> 
            if isNull el && model.mapIsLoaded then
                dispatch MapDivUnloaded

            if not (isNull el) then
                if not model.mapIsLoaded && el.id.Length < 1 then
                    el.id <- mapDivId()
                    el.id |> MapDivLoaded |> dispatch
          )
          Style [ CSSProp.Height "400px" 
                  CSSProp.Width "300px"] ] 
        [ ]