module Main exposing (..)

import Browser
import Canvas
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Color exposing (Color)
import Html exposing (Attribute, Html, b, button, div, h1, input, p, text)
import Html.Attributes as Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { title : String
    , secondsLeft : Int
    , isRuning : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "Time Timer" 3600 False
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | StartStopTimer
    | SetStartTime String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.isRuning then
                if model.secondsLeft > 0 then
                    ( { model | secondsLeft = model.secondsLeft - 1 }
                    , Cmd.none
                    )

                else
                    ( { model | secondsLeft = 0, isRuning = False }
                    , Cmd.none
                    )

            else
                ( model, Cmd.none )

        StartStopTimer ->
            ( { model | isRuning = not model.isRuning }, Cmd.none )

        SetStartTime value ->
            ( { model | secondsLeft = parseMinutesInput model.secondsLeft value }
            , Cmd.none
            )


parseMinutesInput : Int -> String -> Int
parseMinutesInput default value =
    case String.toInt value of
        Just minutes ->
            if minutes > 60 then
                60 * 60

            else if minutes >= 0 then
                minutes * 60

            else
                0

        Nothing ->
            default



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


w =
    500


h =
    500


view : Model -> Html Msg
view model =
    let
        center =
            ( w / 2, h / 2 )

        radius =
            160
    in
    div bodyStyle
        [ h1 centerHeadlineStyle [ Html.text model.title ]
        , div (pieChartInputStyle (round (2 * Tuple.first center)))
            [ Canvas.toHtml
                ( w, h )
                []
                (List.append (renderText center radius) (renderPieChart model.secondsLeft center radius))
            , div [ style "display" "flex", style "flex-direction" "row", style "align-items" "baseline", style "gap" "1rem" ]
                [ input [ type_ "number", Attributes.placeholder "60", onInput SetStartTime ] []
                , text "min"
                ]
            , p []
                [ text "Time left: "
                , b [] [ text (String.fromInt (floor (toFloat model.secondsLeft / 60)) ++ " min") ]
                , text " and "
                , b [] [ text (String.fromInt (modBy 60 model.secondsLeft) ++ " s") ]
                ]
            , button [ onClick StartStopTimer ]
                [ if not model.isRuning then
                    text "Start"

                  else
                    text "Stop"
                ]
            ]
        ]


renderText : ( Float, Float ) -> Float -> List Canvas.Renderable
renderText center radius =
    [ Canvas.text textStyle
        ( Tuple.first center, Tuple.second center - radius - 2 )
        "0"
    , Canvas.text textStyle
        ( Tuple.first center + radius + 14, Tuple.second center + 8 )
        "15"
    , Canvas.text textStyle
        ( Tuple.first center, Tuple.second center + radius + 20 )
        "30"
    , Canvas.text textStyle
        ( Tuple.first center - radius - 16, Tuple.second center + 8 )
        "45"
    ]


textStyle : List Setting
textStyle =
    [ font { size = 24, family = "roboto" }, align Center, fill (Color.fromRgba { red = 96 / 255, green = 108 / 255, blue = 118 / 255, alpha = 1 }) ]


renderPieChart : Int -> ( Float, Float ) -> Float -> List Canvas.Renderable
renderPieChart secondsLeft center radius =
    let
        redDegrees =
            (toFloat secondsLeft / 3600) * 360
    in
    if 270 < redDegrees then
        [ renderPieSlice Color.lightGray center radius (degrees 0) (degrees (270 - redDegrees))
        , renderPieSlice Color.lightRed center radius (degrees (270 - redDegrees)) (degrees 0)
        , renderPieSlice Color.lightRed center radius (degrees 0) (degrees 270)
        ]

    else
        [ renderPieSlice Color.lightGray center radius (degrees 270) (degrees (270 - redDegrees))
        , renderPieSlice Color.lightRed center radius (degrees (270 - redDegrees)) (degrees 270)
        ]


renderPieSlice : Color -> ( Float, Float ) -> Float -> Float -> Float -> Canvas.Renderable
renderPieSlice color (( x, y ) as center) radius startAngle endAngle =
    Canvas.shapes [ fill color ]
        [ Canvas.path center
            [ Canvas.lineTo ( x + radius * cos startAngle, y + radius * sin startAngle )
            , Canvas.lineTo ( x + radius * cos endAngle, y + radius * sin endAngle )
            , Canvas.lineTo center
            ]
        , Canvas.arc
            center
            radius
            { startAngle = startAngle
            , endAngle = endAngle
            , clockwise = True
            }
        ]


bodyStyle : List (Attribute msg)
bodyStyle =
    [ style "margin" "2rem"
    , style "font-family" "roboto"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    ]


pieChartInputStyle : Int -> List (Attribute msg)
pieChartInputStyle width =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    , style "gap" "1rem"
    , style "max-width" (String.fromInt width ++ "px")
    ]


centerHeadlineStyle : List (Attribute msg)
centerHeadlineStyle =
    [ style "display" "flex"
    , style "flex-direction" "row"
    , style "justify-content" "center"
    ]
