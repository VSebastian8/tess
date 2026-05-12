port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, id, name, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import RuleBased.Isogonal exposing (isogonalTesselations)
import RuleBased.Laves exposing (lavesTesselations)
import RuleBased.Regular exposing (regularTesselations)
import RuleBased.Semiregular exposing (semiregularTesselations)
import Rules exposing (..)
import String exposing (fromFloat)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Task
import Util exposing (..)



-- Outgoing ports to JavaScript


port downloadSvg : String -> Cmd msg


port setLocal : ( String, String ) -> Cmd msg


port setCategories : List String -> Cmd msg



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { selectedTess : String
    , toggledCategories : List String
    , selectedTheme : String
    , customStroke : String
    , customPrimary : String
    , customSecondary : String
    , customTernary : String
    , customQuart : String
    , animationKey : Int
    , animated : Bool
    , downloadData : Html Never
    }


type alias Flags =
    { tessellation : String, categories : List String, theme : String, stroke : String, primary : String, secondary : String, ternary : String, quart : String }


init : Flags -> ( Model, Cmd msg )
init { tessellation, categories, theme, stroke, primary, secondary, ternary, quart } =
    ( { selectedTheme = theme
      , toggledCategories = categories
      , customStroke = stroke
      , customPrimary = primary
      , customSecondary = secondary
      , customTernary = ternary
      , customQuart = quart
      , selectedTess = tessellation
      , animationKey = 0
      , animated = False
      , downloadData = a [] [ text "loading" ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SelectTess String
    | ToggleCategory String
    | SelectTheme String
    | PickStroke String
    | PickPrimary String
    | PickSecondary String
    | PickTernary String
    | PickQuart String
    | DownloadSvg
    | TriggerDownloadPort
    | SetLocal String String
    | RunAnimation


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTess tessName ->
            ( { model | selectedTess = tessName }, run (SetLocal "tess-tessellation" tessName) )

        ToggleCategory cat ->
            ( { model | toggledCategories = toggle cat model.toggledCategories }, setCategories (toggle cat model.toggledCategories) )

        SelectTheme theme ->
            ( { model | selectedTheme = theme }, run (SetLocal "tess-theme" theme) )

        PickStroke color ->
            ( { model | customStroke = color }, run (SetLocal "tess-stroke" color) )

        PickPrimary color ->
            ( { model | customPrimary = color }, run (SetLocal "tess-primary" color) )

        PickSecondary color ->
            ( { model | customSecondary = color }, run (SetLocal "tess-secondary" color) )

        PickTernary color ->
            ( { model | customTernary = color }, run (SetLocal "tess-ternary" color) )

        PickQuart color ->
            ( { model | customQuart = color }, run (SetLocal "tess-quart" color) )

        DownloadSvg ->
            ( { model | downloadData = currentSvg model False 1920 1080 }, run TriggerDownloadPort )

        TriggerDownloadPort ->
            ( model, downloadSvg (model.selectedTess ++ " " ++ model.selectedTheme) )

        SetLocal key val ->
            ( model, setLocal ( key, val ) )

        RunAnimation ->
            ( { model | animationKey = model.animationKey + 1, animated = True }
            , Cmd.none
            )


toggle : a -> List a -> List a
toggle x xs =
    if List.member x xs then
        List.filter (\y -> y /= x) xs

    else
        x :: xs



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "container"
        , class ("theme" ++ model.selectedTheme)
        ]
        [ tessMenu model
        , themeMenu model
        , settingsMenu model
        , rulesDisplay model
        , tessDisplay model
        , downloadDisplay model
        ]


categoryTessellations : List ( String, List ( String, Tess ) )
categoryTessellations =
    [ ( "Regular", regularTesselations ), ( "Isogonal", isogonalTesselations ), ( "Semiregular", semiregularTesselations ), ( "Laves", lavesTesselations ) ]


tessellations : List ( String, Tess )
tessellations =
    categoryTessellations |> List.concatMap Tuple.second


tessDict : Dict String Tess
tessDict =
    Dict.fromList tessellations


themeOptions : List String
themeOptions =
    [ "Forest", "Aqua", "Amethyst", "Honey", "Ruby", "Custom" ]


tessMenu : Model -> Html Msg
tessMenu model =
    div
        [ id "menuContainer" ]
        [ div [ id "menuContent" ]
            (categoryTessellations
                |> List.map
                    (\( cat, tessOptions ) ->
                        div
                            [ class "categoryContainer"
                            , if List.member cat model.toggledCategories then
                                class "shownCategory"

                              else
                                class "hiddenCategory"
                            ]
                            (categoryOption cat model
                                :: (tessOptions
                                        |> List.map Tuple.first
                                        |> List.map (\t -> tessOption t model)
                                   )
                            )
                    )
            )
        ]


categoryOption : String -> Model -> Html Msg
categoryOption category model =
    a
        [ class "categoryTitle"
        , onClick (ToggleCategory category)
        ]
        [ text
            (if List.member category model.toggledCategories then
                "∨ " ++ category

             else
                "> " ++ category
            )
        ]


tessOption : String -> Model -> Html Msg
tessOption tessName model =
    a
        [ class "menuItem"
        , class
            (if model.selectedTess == tessName then
                "selected"

             else
                ""
            )
        , onClick (SelectTess tessName)
        ]
        [ text tessName ]


themeMenu : Model -> Html Msg
themeMenu model =
    div
        [ id "themeContainer" ]
        (h2 []
            [ text "Theme" ]
            :: (themeOptions
                    |> List.map (\t -> themeOption t model)
               )
            ++ (if model.selectedTheme == "Custom" then
                    [ colorPicker Primary "Primary" model
                    , colorPicker Secondary "Secondary" model
                    , colorPicker Ternary "Ternary" model
                    , colorPicker Quart "Quart" model
                    , colorPicker Stroke "Stroke" model
                    ]

                else
                    []
               )
        )


themeOption : String -> Model -> Html Msg
themeOption themeName model =
    a
        [ class "menuItem"
        , class
            (if model.selectedTheme == themeName then
                "selected"

             else
                ""
            )
        , onClick (SelectTheme themeName)
        ]
        [ text themeName ]


colorPicker : Color -> String -> Model -> Html Msg
colorPicker colorType labelText model =
    label
        [ class "customColor" ]
        [ input
            [ type_ "color"
            , name "color-picker"
            , value
                (case colorType of
                    Primary ->
                        model.customPrimary

                    Secondary ->
                        model.customSecondary

                    Ternary ->
                        model.customTernary

                    Quart ->
                        model.customQuart

                    Stroke ->
                        model.customStroke
                )
            , onInput
                (\value ->
                    case colorType of
                        Primary ->
                            PickPrimary value

                        Secondary ->
                            PickSecondary value

                        Ternary ->
                            PickTernary value

                        Quart ->
                            PickQuart value

                        Stroke ->
                            PickStroke value
                )
            ]
            []
        , text labelText
        ]


settingsMenu : Model -> Html Msg
settingsMenu model =
    div
        [ id "settingsContainer" ]
        [ button
            [ class "action-btn"
            , onClick RunAnimation
            , class ("theme" ++ model.selectedTheme)
            ]
            [ span [ class "icon" ] [ text "|>" ]
            , span [ class "tooltip" ] [ text "Run animation" ]
            ]
        , button
            [ class "action-btn"
            , onClick DownloadSvg
            , class ("theme" ++ model.selectedTheme)
            ]
            [ span [ class "icon", class "overline" ] [ text "v" ]
            , span [ class "tooltip" ] [ text "Download SVG" ]
            ]
        ]


showTess : Tess -> Bool -> Float -> Float -> Html msg
showTess tess animated w h =
    svg
        [ viewBox ("0 0 " ++ fromFloat w ++ " " ++ fromFloat h)
        , width (fromFloat w)
        , height (fromFloat h)
        , Svg.Attributes.class "tess-svg"
        ]
        (renderTess
            (fix
                (placeStart tess w h)
                ( { x = -3, y = -3 }, { x = w / tess.size + 4, y = h / tess.size + 4 } )
            )
            animated
        )


currentSvg : Model -> Bool -> Float -> Float -> Html Never
currentSvg model animated w h =
    case Dict.get model.selectedTess tessDict of
        Nothing ->
            div [] []

        Just tess ->
            showTess tess animated w h


tessDisplay : Model -> Html msg
tessDisplay model =
    div
        [ id "tessContainer" ]
        [ h2 [ id "tess-text" ] [ text (model.selectedTess ++ " Tessellation") ]
        , Html.Keyed.node "div"
            [ id "tess" ]
            [ ( String.fromInt model.animationKey
              , map never (currentSvg model model.animated 800 800)
              )
            ]
        ]


rulesDisplay : Model -> Html msg
rulesDisplay model =
    div
        [ id "rulesContainer" ]
        [ h2 [] [ text "Rules" ]
        , div [ id "rules" ]
            (case Dict.get model.selectedTess tessDict of
                Nothing ->
                    []

                Just tess ->
                    tess.rules
                        |> List.map (\r -> renderRule r)
            )
        ]


downloadDisplay : Model -> Html msg
downloadDisplay model =
    div
        [ id "tilingDownload", style "display" "none" ]
        [ div [ id "tess" ]
            [ map never model.downloadData
            ]
        ]
