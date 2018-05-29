module Autosuggest

open Elmish
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React
open Fable.Core
open Fable.PowerPack

type FetchRequest = { value : string; reason: string }
type SuggestionSelectedEvent = { suggestion : string; suggestionIndex: int }

type OnSuggestionsFetchRequestedFunction = FetchRequest -> unit
type OnSuggestionsClearRequestedFunction = unit -> unit
type OnSuggestionSelectedFunction = obj -> SuggestionSelectedEvent -> unit
type GetSuggestionValueFunction = string -> string
type RenderSuggestionFunction = string -> React.ReactElement
type OnChangeData = { method : string; newValue: string }
type InputPropsDefinition = {
    value : string
    onChange : Browser.Event -> OnChangeData -> unit 
}

type AutosuggestProps = 
    | Suggestions of string array 
    | OnSuggestionsFetchRequested of OnSuggestionsFetchRequestedFunction 
    | OnSuggestionsClearRequested of OnSuggestionsClearRequestedFunction
    | OnSuggestionSelected of OnSuggestionSelectedFunction
    | GetSuggestionValue of GetSuggestionValueFunction
    | RenderSuggestion of RenderSuggestionFunction
    | InputProps of InputPropsDefinition

let inline AutosuggestComponent (props : AutosuggestProps list) (elems : React.ReactElement list) : React.ReactElement =
    ofImport "default" "react-autosuggest" (keyValueList CaseRules.LowerFirst props) elems

type Model = {
    Suggestions : string array
    Value : string
    SelectedSuggestion : string
}

type Msg = 
    | SuggestionsFetchRequested of string
    | SuggestionsClearRequested
    | OnInputChanges of string
    | OnResultsFetchSuccess of string array
    | OnResultsFetchError of exn
    | OnSuggestionSelectedValue of string

let searchPlaces (query : string) = 
    let executor = fun resolver rejector -> 
        let mutable request = createEmpty<GoogleMaps.Google.Maps.Places.AutocompletionRequest>
        request.input <- query
        GoogleMap.autocompleteService.getPlacePredictions (request, (fun response status -> 
            if not (status.Equals "OK")  then
                System.Exception status |> rejector
            else
                response.ToArray () 
                    |> Array.map (fun item -> item.description)
                    |> resolver
        ))

    Promise.create executor

let init () : Model = { Suggestions = [||]; Value = ""; SelectedSuggestion = "" }

let update (msg : Msg) (model: Model) : Model*Cmd<Msg> =
    match msg with
    | SuggestionsClearRequested -> 
        { model with Suggestions = [||] }, Cmd.none
    | SuggestionsFetchRequested query ->
        model, Cmd.ofPromise searchPlaces query OnResultsFetchSuccess OnResultsFetchError
    | OnInputChanges value -> 
        { model with Value = value }, Cmd.none
    | OnResultsFetchSuccess results -> 
        { model with Suggestions = results }, Cmd.none
    | OnResultsFetchError exn -> 
        Browser.console.log exn
        { model with Suggestions = [||]}, Cmd.none
    | OnSuggestionSelectedValue value -> 
        { model with SelectedSuggestion = value }, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) = 
    let props = {
        value = model.Value
        onChange = ( fun _ value -> OnInputChanges value.newValue |> dispatch )
    }
    AutosuggestComponent [
        Suggestions model.Suggestions
        OnSuggestionsClearRequested (fun _ -> dispatch SuggestionsClearRequested )
        OnSuggestionsFetchRequested (fun request -> SuggestionsFetchRequested request.value |> dispatch )
        GetSuggestionValue (fun suggestion -> suggestion)
        RenderSuggestion (fun suggestion -> b [] [ str suggestion] )
        OnSuggestionSelected (fun _ ev -> OnSuggestionSelectedValue ev.suggestion |> dispatch )
        InputProps props
    ] []