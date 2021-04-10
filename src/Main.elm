
module Main exposing (..)


import Browser
import Html exposing (Html)
import List exposing (concat)
import String exposing (join)
import Svg exposing (..)
import Svg.Attributes as Attribute exposing (..)
import Task
import Time

radius : Float
radius = 350
radiusStr = radius |> String.fromFloat
diameter = radius*2
diameterStr = radius*2 |> String.fromFloat

sunDiameter : Float
sunDiameter = 60
sunRadius = sunDiameter / 2

center = sunDiameter + radius

dotColor = "black"
clockFaceColor = "#ff00888b"
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
  { zone : Time.Zone
  , time : Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0)
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform Tick Time.now
      ]
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 10 Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    sizeStr = String.fromFloat (diameter+sunDiameter*2)
    centerStr = String.fromFloat (sunDiameter + radius)
  in
  svg
    [ (join " " ["0", "0", sizeStr, sizeStr]) |> viewBox
    , width sizeStr
    , height sizeStr
    ]
    ([ rect [ x "0", y "0", width sizeStr, height sizeStr, fill "blue" ] []
    , circle [ cx centerStr, cy centerStr, r radiusStr, fill clockFaceColor ] []
    ]
    ++ viewHands model
    ++ viewSunAtHourHand (hourTurns model)
    ++ view24Dots)

viewSunAtHourHand : Float -> List (Svg msg)
viewSunAtHourHand turns =
        let
            t = 2 * pi * (turns - 0.25)
            amplitude = radius + sunRadius
            x = center + amplitude * cos t
            y = center + amplitude * sin t
        in
            viewSun x y

viewSun : Float -> Float -> List (Svg msg)
viewSun xCenter yCenter =
    [ (circle [ cx (String.fromFloat xCenter)
             , cy (String.fromFloat yCenter)
             , r (String.fromFloat (sunRadius*0.6))
             , fill "yellow" ]
             [])
    ]
    ++ viewSunRays xCenter yCenter 17

viewSunRays : Float -> Float -> Int -> List (Svg msg)
viewSunRays xCenter yCenter numRays =
    List.range 0 numRays
        |> List.map toFloat
        |> List.map (\i -> viewSunRay xCenter yCenter (i/(toFloat numRays)))

viewSunRay : Float -> Float -> Float -> Svg msg
viewSunRay xCenter yCenter turns =
    viewRay xCenter yCenter 1 sunRadius (sunRadius*0.7) turns "yellow"

hourTurns : Model -> Float
hourTurns model =
    let
        hour   = toFloat (Time.toHour   model.zone model.time)
    in
        (0.5 + hour/24)

viewHands : Model -> List (Svg msg)
viewHands model =
    let
        minute = toFloat (Time.toMinute model.zone model.time) + (toFloat (Time.toSecond model.zone model.time)) / 60
        second = (toFloat (Time.toSecond model.zone model.time)) + (toFloat (Time.toMillis model.zone model.time)) / 1000
    in
        [ viewHand 8 (radius*0.6) (hourTurns model)
        , viewHand 6 (radius*0.9) (minute/60)
        , viewHand 3 (radius*0.9) (second/60)]

viewHand : Int -> Float -> Float -> Svg msg
viewHand width length turns =
    viewRay center center width 0 length turns "red"

viewRay : Float -> Float -> Int -> Float -> Float -> Float -> String -> Svg msg
viewRay xCenter yCenter width radius1 radius2 turns color =
    let
        t = 2 * pi * (turns - 0.25)
        x1_ = xCenter + radius1 * cos t
        y1_ = yCenter + radius1 * sin t
        x2_ = xCenter + radius2 * cos t
        y2_ = yCenter + radius2 * sin t
    in
        line
            [ x1 (String.fromFloat x1_)
            , y1 (String.fromFloat y1_)
            , x2 (String.fromFloat x2_)
            , y2 (String.fromFloat y2_)
            , stroke color
            , strokeWidth (String.fromInt width)
            , strokeLinecap "round"
            ]
            []


view24Dots : List (Svg msg)
view24Dots =
    List.range 0 23
        |> List.map toFloat
        |> List.map (\n -> n/24)
        |> List.map viewDot
        |> List.concat

viewDot : Float -> List (Svg msg)
viewDot turns =
    let
        dotRadius = radius / 50
        t = 2 * pi * (turns - 0.25)
        amplitude = radius - dotRadius
        x = center + amplitude * cos t
        y = center + amplitude * sin t
    in
        [ circle [ cx (String.fromFloat x), cy (String.fromFloat y), r (String.fromFloat dotRadius), fill dotColor, opacity "50%" ] []
        ]
