module DatePicker

open Elmish
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React
open Fable.Core
open Moment.Moment

type OnDateChange = Moment -> unit

type DatepickerProps = 
    | OnChange of OnDateChange 
    | Selected of Moment 

// type RCom = React.ComponentClass<obj>

importDefault "react-datepicker/dist/react-datepicker.min.css"
// let DatePickerReactComponent = from ( importDefault<RCom> "react-datepicker" )

let inline DatePickerReactComponent (props : DatepickerProps list) (elems : React.ReactElement list) : React.ReactElement =
    ofImport "default" "react-datepicker" (keyValueList CaseRules.LowerFirst props) elems

type Model = Moment

type Msg = 
    | DateSelected of Moment

let init () : Model = Moment.moment.now() / 1000.0 |> Moment.moment.unix

let update (msg : Msg) _ =
    match msg with
    | DateSelected x -> x

let view (model : Model) (dispatch : Msg -> unit) = 
    DatePickerReactComponent [
        OnChange (fun date -> dispatch ( DateSelected date ) )
        Selected model
    ] []