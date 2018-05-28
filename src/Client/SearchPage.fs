module SearchPage
module R = Fable.Helpers.React
open Elmish
open Fulma
open Fable.Core.JsInterop

importDefault "bulma/css/bulma.css"

type Model = {
    Autosuggest: Autosuggest.Model
}

type Msg = 
| AutosuggestChange of Autosuggest.Msg

let init() : Model * Cmd<Msg> = 
    let initialState = { 
        Autosuggest = Autosuggest.init()
    }
    initialState, Cmd.none

let update (msg:Msg) (model:Model) : Model * Cmd<Msg> =
    match msg with
    | AutosuggestChange x ->
        let m, _ = Autosuggest.update x model.Autosuggest
        { model with Autosuggest = m }, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    Container.container [ Container.IsFluid ] [ 
        Content.content [] [ 
            Autosuggest.view (model.Autosuggest) (AutosuggestChange >> dispatch) ]
        ]
