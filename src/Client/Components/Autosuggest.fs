module Autosuggest

open Elmish
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React
open Fable.Core

type FetchRequest = { value : string; reason: string }
type OnSuggestionsFetchRequestedFunction = FetchRequest -> unit
type OnSuggestionsClearRequestedFunction = unit -> unit
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
    | GetSuggestionValue of GetSuggestionValueFunction
    | RenderSuggestion of RenderSuggestionFunction
    | InputProps of InputPropsDefinition

let inline AutosuggestComponent (props : AutosuggestProps list) (elems : React.ReactElement list) : React.ReactElement =
    ofImport "default" "react-autosuggest" (keyValueList CaseRules.LowerFirst props) elems

type Model = {
    Suggestions : string array
    Value : string
}

type Msg = 
    | SuggestionsFetchRequested of string
    | SuggestionsClearRequested
    | OnInputChanges of string

let init () : Model = { Suggestions = [||]; Value = "" }

let update (msg : Msg) (model: Model) =
    match msg with
    | SuggestionsClearRequested -> 
        { model with Suggestions = [||] } 
    | SuggestionsFetchRequested query -> 
        Browser.console.log query
        { model with Suggestions = [|query|] }
    | OnInputChanges value -> 
        { model with Value = value }

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
        InputProps props
    ] []