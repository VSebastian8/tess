port module Main exposing (..)

import Browser
import ColorTheme exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, id, name, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import RuleBased.Isogonal exposing (isogonalTesselations)
import RuleBased.Regular exposing (regularTesselations)
import RuleBased.Semiregular exposing (semiregularTesselations)
import Rules exposing (..)
import String exposing (fromFloat)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)



-- Outgoing port to JavaScript


port downloadSvg : String -> Cmd msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { selectedTess : String
    , selectedTheme : String
    , customStroke : String
    , customPrimary : String
    , customSecondary : String
    , customTernary : String
    , customQuart : String
    }


tesselations : List ( String, Tess )
tesselations =
    regularTesselations ++ isogonalTesselations ++ semiregularTesselations


tessOptions : List String
tessOptions =
    tesselations |> List.map Tuple.first


tessDict : Dict String Tess
tessDict =
    Dict.fromList tesselations


themeOptions : List String
themeOptions =
    [ "Forest", "Aqua", "Amethyst", "Honey", "Custom" ]


init : ( Model, Cmd msg )
init =
    ( { selectedTheme = "Forest"
      , customStroke = "#000000"
      , customPrimary = "#FFFFFF"
      , customSecondary = "#CCCCCC"
      , customTernary = "#999999"
      , customQuart = "#555555"
      , selectedTess = "Square Flower"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SelectTess String
    | SelectTheme String
    | PickStroke String
    | PickPrimary String
    | PickSecondary String
    | PickTernary String
    | PickQuart String
    | DownloadSvg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTess tessName ->
            ( { model | selectedTess = tessName }, Cmd.none )

        SelectTheme theme ->
            ( { model | selectedTheme = theme }, Cmd.none )

        PickStroke color ->
            ( { model | customStroke = color }, Cmd.none )

        PickPrimary color ->
            ( { model | customPrimary = color }, Cmd.none )

        PickSecondary color ->
            ( { model | customSecondary = color }, Cmd.none )

        PickTernary color ->
            ( { model | customTernary = color }, Cmd.none )

        PickQuart color ->
            ( { model | customQuart = color }, Cmd.none )

        DownloadSvg ->
            ( model, downloadSvg (model.selectedTess ++ "_" ++ model.selectedTheme) )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "container" ]
        [ tessMenu model
        , themeMenu model
        , settingsMenu
        , rulesDisplay model
        , tessDisplay model
        , downloadDisplay model
        ]


tessMenu : Model -> Html Msg
tessMenu model =
    div
        [ id "menuContainer" ]
        [ div [ id "menuContent" ]
            (tessOptions
                |> List.map (\t -> tessOption t model)
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


settingsMenu : Html Msg
settingsMenu =
    div
        [ id "settingsContainer" ]
        [ a
            [ onClick DownloadSvg
            , class "icon"
            ]
            [ img
                [ src "assets/save.svg"
                , width "50"
                , height "50"
                ]
                []
            ]
        ]


showTess : Tess -> Float -> Float -> Theme -> Html msg
showTess tess w h theme =
    svg
        [ viewBox ("0 0 " ++ fromFloat w ++ " " ++ fromFloat h)
        , width (fromFloat w)
        , height (fromFloat h)
        , style "margin-bottom" "-5px"
        ]
        (renderTess (fix tess ( { x = -3, y = -3 }, { x = w / tess.size + 2, y = h / tess.size + 2 } )) theme)


currentSvg : Model -> Float -> Float -> Html msg
currentSvg model w h =
    case Dict.get model.selectedTess tessDict of
        Nothing ->
            div [] []

        Just tess ->
            showTess tess
                w
                h
                (getTheme
                    model
                )


tessDisplay : Model -> Html msg
tessDisplay model =
    div
        [ id "tessContainer" ]
        [ h2 [] [ text (model.selectedTess ++ " Tessellation") ]
        , div [ id "tess" ]
            [ currentSvg model 800 800
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
                        |> List.map (\r -> renderRule r (getTheme model))
            )
        ]


downloadDisplay : Model -> Html msg
downloadDisplay model =
    div
        [ id "tilingDownload", style "display" "none" ]
        [ div [ id "tess" ]
            [ currentSvg model 1920 1080
            ]
        ]


getTheme : Model -> Theme
getTheme model =
    case model.selectedTheme of
        "Amethyst" ->
            amethystTheme

        "Aqua" ->
            aquaTheme

        "Honey" ->
            honeyTheme

        "Forest" ->
            forestTheme

        _ ->
            { getColor =
                \color ->
                    case color of
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
            }
