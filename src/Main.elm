
module Main exposing (..)


import Browser
import Html exposing (Html)
import List
import String exposing (join)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time

watchFaceRadius : Float
watchFaceRadius = 350
watchFaceDiameter = watchFaceRadius*2

sunPadding : Float
sunPadding = 20

sunDiameter : Float
sunDiameter = 60
sunRadius = sunDiameter / 2

sunTotalSize : Float
sunTotalSize = sunPadding*2 + sunDiameter

center = (sunTotalSize + watchFaceRadius, sunTotalSize + watchFaceRadius)
sizeStr = String.fromFloat (watchFaceDiameter+sunTotalSize*2)
centerStr = String.fromFloat (sunTotalSize + watchFaceRadius)

earthRadius = watchFaceRadius*200

frameColor = "black"
clockFaceColor = "#DF00FF77"
handColor = "black"


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
  , sunriseTurns : Float
  , sunsetTurns : Float
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) (0.5 + 5/24) (0.5 + 20/24)
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
  svg
    [ (join " " ["0", "0", sizeStr, sizeStr]) |> viewBox
    , width sizeStr
    , height sizeStr
    ]
    (
    viewBackground
    ++ viewSunsetAndSunriseDots model
    ++ viewEarth model
    ++ viewWatchFace
    ++ viewHands model
    ++ viewSun (hourTurns model)
    )

turnsToAngle turns = 2 * pi * (turns - 0.25)

sunriseAndSunset model =
    let
        amplitude = watchFaceRadius + sunRadius + sunPadding
        sunriseAngle = turnsToAngle model.sunriseTurns
        sunrisePos = (cos sunriseAngle, sin sunriseAngle) |> mul amplitude |> add center
        sunsetAngle = turnsToAngle model.sunsetTurns
        sunsetPos = (cos sunsetAngle, sin sunsetAngle) |> mul amplitude |> add center
    in
        ( sunrisePos
        , sunsetPos
        )

viewEarth : Model -> List (Svg msg)
viewEarth model =
    let
        earthMaybe = earthPosition model
    in
        case earthMaybe of
            Just pos -> viewEarthAt pos
            Nothing -> []

viewEarthAt : (Float, Float) -> List (Svg msg)
viewEarthAt (x, y) =
    [ circle
      [ cx (String.fromFloat x)
      , cy (String.fromFloat y)
      , r (String.fromFloat earthRadius)
      , fill "#ffffff99"
      ]
      []
    ]


earthPosition : Model -> Maybe (Float, Float)
earthPosition model =
    let
        (sunrise, sunset) = sunriseAndSunset model
        maybeEarth = circlePoints sunrise sunset earthRadius
    in
        case maybeEarth of
          Just (earth1, earth2) -> Just <| lowest earth1 earth2
          Nothing -> Nothing


lowest : (Float, Float) -> (Float, Float) -> (Float, Float)
lowest (x2, y1) (x1, y2) =
    if y1 > y2 then
        (x1, y1)
    else
        (x2, y2)

viewSunsetAndSunriseDots : Model -> List (Svg msg)
viewSunsetAndSunriseDots model =
    let
        ((xSunrise, ySunrise), (xSunset, ySunSet)) = sunriseAndSunset model
        (midX, midY) = div2 (add (xSunrise, ySunrise) (xSunset, ySunSet)) 2
    in
        [ circle -- midpoint between sunrise and sunset
            [ cx (String.fromFloat midX)
            , cy (String.fromFloat midY)
            , r "20"
            , fill "green"
            ]
            []
        , circle
            [ cx (String.fromFloat xSunrise)
            , cy (String.fromFloat ySunrise)
            , r "20"
            , fill "white"
            ]
            []
        , circle
            [ cx (String.fromFloat xSunset)
            , cy (String.fromFloat ySunSet)
            , r "20"
            , fill "black"
            ]
            []
        ]

viewBackground : List (Svg msg)
viewBackground =
    [ rect [ x "0", y "0", width sizeStr, height sizeStr, fill "blue" ] []
    ]

viewWatchFace : List (Svg msg)
viewWatchFace =
    [ circle [ cx centerStr, cy centerStr, r (String.fromFloat watchFaceRadius), fill clockFaceColor, stroke frameColor, strokeWidth "4" ] []
    ]
    ++ view24Dots
    ++ view12Dots

viewSun : Float -> List (Svg msg)
viewSun turns =
    let
        amplitude = watchFaceRadius + sunRadius + sunPadding
        pos = (cos (turnsToAngle turns), sin (turnsToAngle turns)) |> mul amplitude |> add center
    in
        viewSunAt pos

viewSunAt : (Float, Float) -> List (Svg msg)
viewSunAt (xCenter, yCenter) =
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
    viewRay (xCenter, yCenter) 1 sunRadius (sunRadius*0.7) turns "yellow"

hourTurns : Model -> Float
hourTurns model =
    let
        hour = toFloat (Time.toHour model.zone model.time) + (toFloat (Time.toMinute model.zone model.time) + (toFloat (Time.toSecond model.zone model.time)) / 60) / 60
    in
        (0.5 + hour/24)

viewHands : Model -> List (Svg msg)
viewHands model =
    let
        minute = toFloat (Time.toMinute model.zone model.time) + (toFloat (Time.toSecond model.zone model.time)) / 60
        second = (toFloat (Time.toSecond model.zone model.time)) + (toFloat (Time.toMillis model.zone model.time)) / 1000
    in
        [ viewHand 8 (watchFaceRadius*0.6) (hourTurns model)
        , viewHand 6 (watchFaceRadius*0.9) (minute/60)
        , viewHand 3 (watchFaceRadius*0.9) (second/60)
        , circle [ cx centerStr, cy centerStr, r "10", fill handColor ] []
        ]

viewHand : Int -> Float -> Float -> Svg msg
viewHand width length turns =
    viewRay center width 0 length turns handColor

viewRay : (Float, Float) -> Int -> Float -> Float -> Float -> String -> Svg msg
viewRay (xCenter, yCenter) width radius1 radius2 turns color =
    let
        x1_ = xCenter + radius1 * cos (turnsToAngle turns)
        y1_ = yCenter + radius1 * sin (turnsToAngle turns)
        x2_ = xCenter + radius2 * cos (turnsToAngle turns)
        y2_ = yCenter + radius2 * sin (turnsToAngle turns)
    in
        line
            [ x1 (String.fromFloat x1_)
            , y1 (String.fromFloat y1_)
            , x2 (String.fromFloat x2_)
            , y2 (String.fromFloat y2_)
            , stroke color
            , strokeWidth (String.fromInt width)
            --, strokeLinecap "round"
            ]
            []


view24Dots : List (Svg msg)
view24Dots =
    List.range 0 23
        |> List.map toFloat
        |> List.map (\n -> n/24)
        |> List.map (viewDot (watchFaceRadius / 25))
        --|> List.concat

view12Dots : List (Svg msg)
view12Dots =
    List.range 0 12
        |> List.map toFloat
        |> List.map (\n -> n/12)
        |> List.map (viewDot (watchFaceRadius / 12))

viewDot : Float -> Float -> Svg msg
viewDot length turns =
    viewRay center 4 watchFaceRadius (watchFaceRadius - length) turns frameColor



-- https://rosettacode.org/wiki/Circles_of_given_radius_through_two_points#Haskell

add (a, b) (x, y) = (a + x, b + y)
sub (a, b) (x, y) = (a - x, b - y)
magSqr (a, b)     = (a ^ 2) + (b ^ 2)
mag a             = sqrt (magSqr a)
mul c (a, b)      = (a * c, b * c)
div2 (a, b) c     = (a / c, b / c)
perp (a, b)       = (negate b, a)
norm a            = div2 a (mag a)

circlePoints : (Float, Float) -> (Float, Float) -> Float -> Maybe ((Float, Float), (Float, Float))
circlePoints p q radius =
    let
        diameter = radius * 2
        pq       = sub p q
        magPQ    = mag pq
        midpoint = div2 (add p q) 2
        halfPQ   = magPQ / 2
        magMidC  = sqrt (abs ((radius ^ 2) - (halfPQ ^ 2)))
        midC     = mul magMidC (norm (perp pq))
        center1  = add midpoint midC
        center2  = sub midpoint midC
    in
        if radius == 0 then
            Nothing
        else if p == q  then
            Nothing
        else if diameter < magPQ  then
            Nothing
        else
            Just (center1, center2)

