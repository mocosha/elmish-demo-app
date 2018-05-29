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

type Page = Home | Search

type Model = {
    Route: Page
    Query: string
    Uri: string
    HomePageModel: HomePage.Model
    SearchPageModel: SearchPage.Model
}

type Msg = 
    | RouteChange of Page
    | HomePageLoad of HomePage.Msg
    | SearchPageMsg of SearchPage.Msg

let pageToUri page =
    match page with
    | Home -> "#home"
    | Search -> "#search"

let uriToPage uri =
    match uri with
    | "#home" -> Home
    | "#search" -> Search
    | _ -> Home

let urlUpdate (result: Option<Page>) (model : Model) : Model * Cmd<Msg> =
    match result with
        | Some page ->
            { model with Route = page; Query = "" }, []
        | None ->
            let page = uriToPage Browser.location.hash
            let uri = pageToUri page
            { model with Route = page; Uri = uri }, Navigation.modifyUrl uri

let init initRoute = 
    let homePageInitState, _ = HomePage.init()
    let searchPageInitState, _ = SearchPage.init()
    let model = { Route = Home; Query = ""; Uri = pageToUri Home; HomePageModel = homePageInitState; SearchPageModel = searchPageInitState }
    urlUpdate initRoute model

let update (msg:Msg) (model:Model) =
    match msg with
                | RouteChange x ->
                    { model with Route = x; Uri = pageToUri x }, pageToUri x |> Navigation.newUrl
                | HomePageLoad x -> 
                    let homePageModel, _ = HomePage.update x model.HomePageModel
                    { model with HomePageModel = homePageModel }, Cmd.none
                | SearchPageMsg x -> 
                    let searchPageModel, cmd = SearchPage.update x model.SearchPageModel
                    { model with SearchPageModel = searchPageModel }, Cmd.map SearchPageMsg cmd
                    
let view (model : Model) (dispatch : Msg -> unit) =
    let child = match model.Route with 
                | Home _ -> 
                    R.div   [ R.classList [ "block", true ] ]
                            [ Button.a 
                                [ Button.Props [
                                    OnClick (fun _ -> 
                                        Search |> RouteChange |> dispatch
                                    ) 
                                ] ] 
                                [ R.str "To search" ] 
                              HomePage.view model.HomePageModel (HomePageLoad >> dispatch) ]
                | Search _ -> 
                    R.div   [ R.classList [ "block", true ] ]
                            [ Button.a 
                                [ Button.Props [
                                    OnClick (fun _ -> 
                                        Home |> RouteChange |> dispatch
                                    ) 
                                ] ] 
                                [ R.str "Back" ] 
                              SearchPage.view model.SearchPageModel (SearchPageMsg >> dispatch)]

    Container.container [ Container.IsFluid ] [ Content.content [] [ child ] ]

let route =
    oneOf
        [ map Home (s (pageToUri Home))
          map Search (s (pageToUri Search)) ]

let urlParser location =
    parseHash route location

Program.mkProgram init update view
|> Program.toNavigable urlParser urlUpdate
|> Program.withConsoleTrace
|> Program.withReact "elmish-app"
|> Program.run