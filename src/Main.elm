port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, id, name, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import RuleBased.Isogonal exposing (isogonalTesselations)
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
    , selectedTheme : String
    , customStroke : String
    , customPrimary : String
    , customSecondary : String
    , customTernary : String
    , customQuart : String
    , animationKey : Int
    , animated : Bool
    }


type alias Flags =
    { tessellation : String, theme : String, stroke : String, primary : String, secondary : String, ternary : String, quart : String }


init : Flags -> ( Model, Cmd msg )
init { tessellation, theme, stroke, primary, secondary, ternary, quart } =
    ( { selectedTheme = theme
      , customStroke = stroke
      , customPrimary = primary
      , customSecondary = secondary
      , customTernary = ternary
      , customQuart = quart
      , selectedTess = tessellation
      , animationKey = 0
      , animated = False
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
            ( model, downloadSvg (model.selectedTess ++ " " ++ model.selectedTheme) )

        SetLocal key val ->
            ( model, setLocal ( key, val ) )

        RunAnimation ->
            ( { model | animationKey = model.animationKey + 1, animated = True }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "container"
        , class ("theme" ++ model.selectedTheme)
        ]
        [ tessMenu model
        , themeMenu model
        , settingsMenu
        , rulesDisplay model
        , tessDisplay model
        , downloadDisplay model
        ]


tessellations : List ( String, Tess )
tessellations =
    regularTesselations ++ isogonalTesselations ++ semiregularTesselations


tessOptions : List String
tessOptions =
    tessellations |> List.map Tuple.first


tessDict : Dict String Tess
tessDict =
    Dict.fromList tessellations


themeOptions : List String
themeOptions =
    [ "Forest", "Aqua", "Amethyst", "Honey", "Custom" ]


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
        [ button
            [ class "action-btn", onClick RunAnimation ]
            [ span [ class "icon" ] [ text "|>" ]
            , span [ class "tooltip" ] [ text "Run animation" ]
            ]
        , button
            [ class "action-btn", onClick DownloadSvg ]
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
        , style "margin-bottom" "-5px"
        ]
        (renderTess
            (fix tess ( { x = -3, y = -3 }, { x = w / tess.size + 2, y = h / tess.size + 2 } ))
            animated
        )


currentSvg : Model -> Bool -> Float -> Float -> Html msg
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
        [ h2 [] [ text (model.selectedTess ++ " Tessellation") ]
        , Html.Keyed.node "div"
            [ id "tess" ]
            [ ( String.fromInt model.animationKey
              , currentSvg model model.animated 800 800
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
            [ currentSvg model False 1920 1080
            ]
        ]
