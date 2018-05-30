module HomePage
module R = Fable.Helpers.React
open Elmish
open Fulma
open Fable.Import
open Fable.Core.JsInterop
open Fable.Helpers.React.Props

importDefault "bulma/css/bulma.css"

type Model = {
    Map: MapComponent.Model
    SecondMap: MapComponent.Model
    DatepickerFirst: DatePicker.Model
    DatepickerSecond: DatePicker.Model
}

type Msg = 
| MapComponentMsg of MapComponent.Msg
| SecondMapComponentMsg of MapComponent.Msg
| DatepPickerFirstDateChange of DatePicker.Msg
| DatepPickerSecondDateChange of DatePicker.Msg

let init() : Model * Cmd<Msg>= 
    let datepickerFirstState = DatePicker.init()
    let datepickerSecondState = DatePicker.init()
    let mapComponentState, _ = MapComponent.init()
    let initialState = { 
        Map = { mapComponentState with options = { mapComponentState.options with zoom = 6.0m } }
        SecondMap = { mapComponentState with options = { mapComponentState.options with zoom = 8.0m } }
        DatepickerFirst = datepickerFirstState
        DatepickerSecond = datepickerSecondState
    }
    initialState, Cmd.batch [ 
        Cmd.map DatepPickerFirstDateChange Cmd.none
        Cmd.map DatepPickerSecondDateChange Cmd.none ]

let update (msg:Msg) (model:Model) : Model * Cmd<Msg> =
    match msg with
    | MapComponentMsg x ->
        let mapModel, _ = MapComponent.update x model.Map
        { model with Map = mapModel }, Cmd.none
    | SecondMapComponentMsg x ->
        let mapModel, _ = MapComponent.update x model.SecondMap
        { model with SecondMap = mapModel }, Cmd.none
    | DatepPickerFirstDateChange x ->
        let datepickerFirstModel = DatePicker.update x model.DatepickerFirst
        { model with DatepickerFirst = datepickerFirstModel }, Cmd.none
    | DatepPickerSecondDateChange x ->
        let datepickerSecondModel = DatePicker.update x model.DatepickerSecond
        { model with DatepickerSecond = datepickerSecondModel; DatepickerFirst = datepickerSecondModel }, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    Container.container [ Container.IsFluid ] [ 
        Content.content    [] [ 
            DatePicker.view (model.DatepickerFirst) (DatepPickerFirstDateChange >> dispatch) 
            DatePicker.view (model.DatepickerSecond) (DatepPickerSecondDateChange >> dispatch) 
            R.div   [ R.classList [ "block", true ] ]
                    [ Button.a [ ] [ R.str "Anchor" ]
                      Button.span [ ] [ R.str "Span" ]
                      Button.button [ ] [ R.str "Button" ] 
                      Button.Input.reset [ Button.Props [ Value "Input `reset`" ] ]
                      Button.Input.submit [ Button.Props [ Value "Input `submit`" ] ]
                      Label.label [] [ R.str "Map 1 zoom" ]
                      Input.number [ Input.Props [ Step "0.1" ] ]
                      MapComponent.view model.Map (MapComponentMsg >> dispatch)
                      Label.label [] [ R.str "Map 2 zoom" ]
                      Input.number [ Input.Props [ Step "0.1"; OnChange (fun a -> Browser.console.log a.target?value) ] ]
                      MapComponent.view model.Map (SecondMapComponentMsg >> dispatch) ]
        ] ]
