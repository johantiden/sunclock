
module Main exposing (..)


import Browser
import Html exposing (Html)
import List exposing (concat)
import String exposing (join)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time
import Tuple exposing (first, second)

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

watchFaceCenter = (sunTotalSize + watchFaceRadius, sunTotalSize + watchFaceRadius)
sceneSize = watchFaceDiameter+sunTotalSize*2
sceneSizeStr = String.fromFloat sceneSize
centerStr = String.fromFloat (sunTotalSize + watchFaceRadius)

frameColor = "black"
clockFaceColor = "#FFFFFFFF"
handColor = "black"
earthColor = "#004400ff"


sunriseHour = 5
sunsetHour = 20

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
  ( Model Time.utc (Time.millisToPosix 0) (0.5 + sunriseHour/24) (0.5 + sunsetHour/24)
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
subscriptions _ =
  Time.every 10 Tick

-- VIEW


view : Model -> Html Msg
view model =
  svg
    [ (join " " ["0", "0", sceneSizeStr, sceneSizeStr]) |> viewBox
    , width sceneSizeStr
    , height sceneSizeStr
    ]
    (
    viewBackground
    ++ viewSun (hourTurns model) "yellow" "none"
    ++ viewEarth_ model
    ++ viewSun (hourTurns model) "#ffff0011" "white"
    ++ viewWatchFace
    ++ viewHands model
    )

turnsToAngle turns = 2 * pi * (turns - 0.25)

sunriseAndSunset model =
    let
        amplitude = watchFaceRadius + sunRadius + sunPadding
        (sunriseAngle, sunsetAngle) = sunriseAndSunsetAngle model
        sunrisePos = radial watchFaceCenter amplitude sunriseAngle
        sunsetPos = radial watchFaceCenter amplitude sunsetAngle
    in
        ( sunrisePos
        , sunsetPos
        )

radial center radius angle  =
    (cos angle, sin angle) |> mul radius |> add center

sunriseAndSunsetAngle model =
    ( turnsToAngle model.sunriseTurns
    , turnsToAngle model.sunsetTurns
    )

viewEarth_ : Model -> List (Svg msg)
viewEarth_ model =
    let
        (sunrise, sunset) = sunriseAndSunset model
        --midpoint = div2 (add sunrise sunset) 2
        --p = sunrise
        --q = sunset
        --pq       = sub p q
        --normal_ = normalize (perp pq)
        --center1  = add midpoint (mul 100 normal_)
        --center2  = sub midpoint (mul 100 normal_)

        --amplitude = watchFaceRadius + sunRadius + sunPadding
        --midAngle = (v1 + v2) /2
        --midPos = (cos midAngle, sin midAngle) |> mul amplitude |> add watchFaceCenter
        --(v1, v2) = sunriseAndSunsetAngle model
    in
        [
          --viewCircle midpoint 10 "green"
        --, viewCircle sunrise 10 "white"
        --, viewCircle sunset 10 "black"
        --, viewCircle center1 10 "green"
        --, viewCircle center2 10 "green"
        --, viewCircle midPos 10 "cyan"
        ] ++ viewEarth sunrise sunset earthColor "none"

viewCircle center radius fill_ =
    circle
        [ cx <| String.fromFloat <| first center
        , cy <| String.fromFloat <| second center
        , r <| String.fromFloat radius
        , fill fill_
        ]
        []

viewBackground : List (Svg msg)
viewBackground =
    [ rect [ x "0", y "0", width sceneSizeStr, height sceneSizeStr, fill "blue" ] []
    ]

viewWatchFace : List (Svg msg)
viewWatchFace =
    [ circle [ cx centerStr, cy centerStr, r (String.fromFloat watchFaceRadius), fill clockFaceColor, stroke frameColor, strokeWidth "4" ] []
    ]
    ++ view24Dots
    ++ view12Dots
    ++ viewNumbers


viewSun : Float -> String -> String -> List (Svg msg)
viewSun turns fill stroke =
    let
        amplitude = watchFaceRadius + sunRadius + sunPadding
        angle = turnsToAngle turns
        pos = radial watchFaceCenter amplitude angle
    in
        viewSunAt pos fill stroke

viewSunAt : (Float, Float) -> String -> String -> List (Svg msg)
viewSunAt (xCenter, yCenter) fill_ stroke_ =
    [ (circle [ cx (String.fromFloat xCenter)
             , cy (String.fromFloat yCenter)
             , r (String.fromFloat (sunRadius*0.6))
             , fill fill_
             , stroke stroke_ ]
             [])
    ]
    ++ viewSunRays xCenter yCenter 17 fill_

viewSunRays : Float -> Float -> Int -> String -> List (Svg msg)
viewSunRays xCenter yCenter numRays color =
    List.range 0 numRays
        |> List.map toFloat
        |> List.map (\i -> viewSunRay xCenter yCenter (i/(toFloat numRays)) color)

viewSunRay : Float -> Float -> Float -> String -> Svg msg
viewSunRay xCenter yCenter turns color =
    viewRay (xCenter, yCenter) 1 sunRadius (sunRadius*0.7) turns color

hourTurns : Model -> Float
hourTurns model =
    let
        --hour = ((toFloat (Time.toSecond model.zone model.time)) + (toFloat (Time.toMillis model.zone model.time)) / 1000) * 2
        hour = toFloat (Time.toHour model.zone model.time) + (toFloat (Time.toMinute model.zone model.time) + (toFloat (Time.toSecond model.zone model.time)) / 60) / 60
    in
        hourTurns_ hour

hourTurns_ : Float -> Float
hourTurns_ hour =
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
viewHand width length_ turns =
    viewRay watchFaceCenter width 0 length_ turns handColor

viewRay : (Float, Float) -> Int -> Float -> Float -> Float -> String -> Svg msg
viewRay center width radius1 radius2 turns color =
    let
        angle = turnsToAngle turns
        (x1_, y1_) = radial center radius1 angle
        (x2_, y2_) = radial center radius2 angle
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


viewNumbers : List (Svg msg)
viewNumbers =
    List.range 0 23
        |> List.map viewNumber
        |> concat

viewNumber : Int -> List (Svg msg)
viewNumber hour =
    let
        angle = hourTurns_ (toFloat hour) |> turnsToAngle
        pos = radial watchFaceCenter (watchFaceRadius*0.8) angle
        fontSize_ =
            if modBy 3 hour == 0 then
              "30"
            else
              "20"
    in
        [ viewCircle pos 1 "yellow"
        , text_
            [ x <| String.fromFloat <| first pos
            , y <| String.fromFloat <| second pos
            , fill "black"
            , textAnchor "middle"
            , dominantBaseline "central"
            , fontSize fontSize_
            ]
            [ text <| String.fromInt hour
            ]
        ]


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
viewDot length_ turns =
    viewRay watchFaceCenter 8 watchFaceRadius (watchFaceRadius - length_) turns frameColor

viewEarth : (Float, Float) -> (Float, Float) -> String -> String -> List (Svg msg)
viewEarth p1 p2 fill stroke =
    let
        yLeft = lineFromTwoPoints p1 p2 0
        yRight = lineFromTwoPoints p1 p2 sceneSize
        earthTopLeft = (0, yLeft)
        earthTopRight = (sceneSize, yRight)
        sceneBottomRight = (sceneSize, sceneSize)
        sceneBottomLeft = (0, sceneSize)
        poly = [earthTopLeft, earthTopRight, sceneBottomRight, sceneBottomLeft]
    in
        viewPolygon fill stroke poly

viewPolygon : String -> String -> List ((Float, Float)) -> List (Svg msg)
viewPolygon fill_ stroke_ poly =
    let
        tupleToString (x, y) = [String.fromFloat x, String.fromFloat y] |> String.join ","
        pointsStr = poly |> List.map tupleToString |> String.join " "
    in
        [ polygon [ fill fill_, stroke stroke_, points pointsStr ] []
        ]

lineFromTwoPoints (x1, y1) (x2, y2) x =
    let
        y = (y2 - y1) * (x - x1) / (x2 - x1) + y1
    in
        y


-- https://rosettacode.org/wiki/Circles_of_given_radius_through_two_points#Haskell

add (a, b) (x, y) = (a + x, b + y)
sub (a, b) (x, y) = (a - x, b - y)
lengthSqr (a, b)     = (a ^ 2) + (b ^ 2)
length a             = sqrt (lengthSqr a)
mul c (a, b)      = (a * c, b * c)
div2 (a, b) c     = (a / c, b / c)
perp (a, b)       = (negate b, a)

normalize : (Float, Float) -> (Float, Float)
normalize a            = div2 a (length a)
