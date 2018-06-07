module App
module R = Fable.Helpers.React
open Elmish
open Elmish.React
open Elmish.Browser
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fulma
open Fable.Import
open Fable.Helpers.React.Props
open Fable.Core.JsInterop

importDefault "bulma/css/bulma.css"

[<RequireQualifiedAccess>]
module Page = 

    type T = 
        | Home 
        | Search
        with 
            member x.AsUri = 
                match x with 
                | Home -> "#home"
                | Search -> "#search"

    let toUri (page: T) = page.AsUri

    let fromUri uri =
        match uri with
        | "#home" -> Home
        | "#search" -> Search
        | _ -> Home    

type Model = 
    { Route: Page.T
      Query: string
      Uri: string
      HomePageModel: HomePage.Model
      SearchPageModel: SearchPage.Model }

type Msg = 
    | RouteChange of Page.T
    | HomePageMsg of HomePage.Msg
    | SearchPageMsg of SearchPage.Msg

let urlUpdate (result: Option<Page.T>) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Some page ->
        { model with Route = page; Query = "" }, []
    | None ->
        let page = Page.fromUri Browser.location.hash
        { model with Route = page; Uri = page.AsUri }, Navigation.modifyUrl page.AsUri

let init initRoute = 
    let homePageInitState, _ = HomePage.init()
    let searchPageInitState, _ = SearchPage.init()
    let model = 
        { Route = Page.Home
          Query = ""
          Uri = Page.Home.AsUri
          HomePageModel = homePageInitState
          SearchPageModel = searchPageInitState }
    urlUpdate initRoute model

let update (msg: Msg) (model: Model) =
    match msg with
    | RouteChange x ->
        let model = 
            { model with 
                Route = x
                Uri = x.AsUri }
            
        model, Navigation.newUrl x.AsUri
    | HomePageMsg x -> 
        let homePageModel, _ = HomePage.update x model.HomePageModel
        { model with HomePageModel = homePageModel }, Cmd.none
    | SearchPageMsg x -> 
        let searchPageModel, cmd = SearchPage.update x model.SearchPageModel
        { model with SearchPageModel = searchPageModel }, Cmd.map SearchPageMsg cmd

let view (model: Model) (dispatch: Msg -> unit) =
    let child = 
        match model.Route with 
        | Page.Home _ -> 
            R.div   [ R.classList [ "block", true ] ]
                    [ Button.a 
                        [ Button.Props [
                            OnClick (fun _ -> 
                                dispatch (RouteChange Page.Search)
                            ) 
                        ] ] 
                        [ R.str "To search" ] 
                      HomePage.view model.HomePageModel (HomePageMsg >> dispatch) ]
        | Page.Search _ -> 
            R.div   [ R.classList [ "block", true ] ]
                    [ Button.a 
                        [ Button.Props [
                            OnClick (fun _ -> 
                                dispatch (RouteChange Page.Home)
                            ) 
                        ] ] 
                        [ R.str "Back" ] 
                      SearchPage.view model.SearchPageModel (SearchPageMsg >> dispatch)]

    Container.container [ Container.IsFluid ] [ Content.content [] [ child ] ]

let route =
    oneOf
        [ map Page.Home (s (Page.Home.AsUri))
          map Page.Search (s (Page.Search.AsUri)) ]

let urlParser location = parseHash route location

Program.mkProgram init update view
|> Program.toNavigable urlParser urlUpdate
|> Program.withConsoleTrace
|> Program.withReact "elmish-app"
|> Program.run