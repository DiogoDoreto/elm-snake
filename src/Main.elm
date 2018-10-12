module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus)
import Json.Decode as Json
import List exposing (head, map, range, reverse, tail)
import Maybe exposing (andThen, withDefault)



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { snake : List Point
    , currentDirection : Direction
    , desiredDirection : Direction
    , running : Bool
    , message : String
    , delta : Float
    }


type alias Point =
    { x : Int
    , y : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (range 1 6 |> reverse |> map (\x -> Point x 1))
        Right
        Right
        False
        "Click here to start"
        0
    , Cmd.none
    )


frameDuration : Float
frameDuration =
    80



-- UPDATE


type Msg
    = Face Direction
    | Pause String
    | Play
    | Tick Float
    | Walk
    | Validate
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pause message ->
            ( { model | running = False, message = message }, Cmd.none )

        Play ->
            ( { model | running = True, message = "", delta = 0 }, Cmd.none )

        Face desiredDirection ->
            ( { model | desiredDirection = desiredDirection }, Cmd.none )

        Tick d ->
            { model | delta = model.delta + d } |> update Walk

        Walk ->
            model |> walk |> update Validate

        Validate ->
            ( validate model, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


validate model =
    if isSnakeInsideBoard model && isSnakeAlive model then
        model

    else
        { model | running = False, message = "Game Over" }


isSnakeInsideBoard { snake } =
    snake
        |> head
        |> andThen (\{ x, y } -> Just (x >= 0 && y >= 0 && x < 40 && y < 40))
        |> withDefault False


isSnakeAlive { snake } =
    case snake of
        snakeHead :: snakeTail ->
            List.all ((/=) snakeHead) snakeTail

        _ ->
            False


walk model =
    if model.delta < frameDuration then
        model

    else
        let
            nextDirection =
                getValidDirection model.currentDirection model.desiredDirection
        in
        { model
            | delta = model.delta - frameDuration
            , snake = walkSnake nextDirection model.snake
            , currentDirection = nextDirection
        }


getValidDirection : Direction -> Direction -> Direction
getValidDirection current wanted =
    case ( current, wanted ) of
        ( Up, Down ) ->
            current

        ( Right, Left ) ->
            current

        ( Down, Up ) ->
            current

        ( Left, Right ) ->
            current

        _ ->
            wanted


walkSnake : Direction -> List Point -> List Point
walkSnake dir snake =
    walkHead dir snake :: walkTail snake


walkHead : Direction -> List Point -> Point
walkHead dir snake =
    snake
        |> head
        |> withDefault (Point 1 1)
        |> (\head ->
                case dir of
                    Right ->
                        { head | x = head.x + 1 }

                    Left ->
                        { head | x = head.x - 1 }

                    Down ->
                        { head | y = head.y + 1 }

                    Up ->
                        { head | y = head.y - 1 }
           )


walkTail : List Point -> List Point
walkTail snake =
    snake
        |> reverse
        |> tail
        |> withDefault []
        |> reverse



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        onAnimationFrameDelta Tick

    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text "@import url('./styles.css')" ]
        , board model
        ]


board : Model -> Html Msg
board model =
    let
        snakeElements =
            map snakeDot model.snake

        overlayDiv =
            div [ class "overlay" ] [ text model.message ]

        pausedClass =
            if model.running then
                ""

            else
                "paused"
    in
    div
        [ class "board"
        , class pausedClass
        , tabindex 0
        , onKeyDown mapDirection
        , onFocus Play
        , onBlur (Pause "Paused - Click here to resume")
        ]
        (overlayDiv :: snakeElements)


snakeDot : Point -> Html Msg
snakeDot point =
    let
        x =
            String.fromInt (point.x * 10)

        y =
            String.fromInt (point.y * 10)

        translate =
            "translate(" ++ x ++ "px, " ++ y ++ "px)"
    in
    div [ class "dot", style "transform" translate ] []


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Html.Events.on "keydown" (Json.map tagger Html.Events.keyCode)


mapDirection : Int -> Msg
mapDirection key =
    case key of
        37 ->
            Face Left

        38 ->
            Face Up

        39 ->
            Face Right

        40 ->
            Face Down

        _ ->
            DoNothing
