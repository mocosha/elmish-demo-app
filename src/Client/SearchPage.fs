module SearchPage
module R = Fable.Helpers.React
open Elmish
open Fulma
open Fable.Core.JsInterop

importDefault "bulma/css/bulma.css"

type Model = {
    AutosuggestPickup: Autosuggest.Model
    AutosuggestDropoff: Autosuggest.Model
}

type Msg = 
| AutosuggestPickupChange of Autosuggest.Msg
| AutosuggestDropoffChange of Autosuggest.Msg

let init() : Model * Cmd<Msg> = 
    let initialState = { 
        AutosuggestPickup = Autosuggest.init()
        AutosuggestDropoff = Autosuggest.init()
    }
    initialState, Cmd.none

let update (msg:Msg) (model:Model) : Model * Cmd<Msg> =
    match msg with
    | AutosuggestPickupChange x ->
        let m, cmd = Autosuggest.update x model.AutosuggestPickup
        { model with AutosuggestPickup = m }, Cmd.map AutosuggestPickupChange cmd
    | AutosuggestDropoffChange x ->
        let m, cmd = Autosuggest.update x model.AutosuggestDropoff
        { model with AutosuggestDropoff = m }, Cmd.map AutosuggestDropoffChange cmd

let view (model : Model) (dispatch : Msg -> unit) =
    Container.container [ Container.IsFluid ] [ 
        Content.content [] [ 
            R.label [] [ R.str "Pickup: "; Autosuggest.view (model.AutosuggestPickup) (AutosuggestPickupChange >> dispatch) ]
            R.p [] [ R.str "Selected pickup address: "; R.b [] [ R.str model.AutosuggestPickup.SelectedSuggestion ] ]
            R.label [] [ R.str "Dropof: "; Autosuggest.view (model.AutosuggestDropoff) (AutosuggestDropoffChange >> dispatch) ]
            R.p [] [ R.str "Selected dropoff address: "; R.b [] [ R.str model.AutosuggestDropoff.SelectedSuggestion ] ] ]
        ]
