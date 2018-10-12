module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus)
import Html.Keyed
import Json.Decode as Json
import List exposing (head, map, range, reverse, tail)
import Maybe exposing (andThen, withDefault)
import Random



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
    , food : Point
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
        (range 1 3 |> reverse |> map (\x -> Point x 1))
        Right
        Right
        False
        "Click here to start"
        0
        (Point 0 0)
    , newFoodCmd
    )


frameDuration : Float
frameDuration =
    80


newFoodCmd : Cmd Msg
newFoodCmd =
    Random.generate
        (\( x, y ) -> Point x y |> PlaceFood)
        (Random.pair (Random.int 0 39) (Random.int 0 39))



-- UPDATE


type Msg
    = Face Direction
    | Pause String
    | Play
    | PlaceFood Point
    | Tick Float
    | Walk
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Pause message ->
            ( { model | running = False, message = message }, Cmd.none )

        Play ->
            ( { model | running = True, message = "", delta = 0 }, Cmd.none )

        Face desiredDirection ->
            ( { model | desiredDirection = desiredDirection }, Cmd.none )

        PlaceFood food ->
            ( { model | food = food }, Cmd.none )

        Tick d ->
            { model | delta = model.delta + d } |> update Walk

        Walk ->
            let
                ( newModel, cmd ) =
                    walk model
            in
            ( validate newModel, cmd )


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
        ( model, Cmd.none )

    else
        let
            nextDirection =
                getValidDirection model.currentDirection model.desiredDirection

            ( newSnake, cmd ) =
                walkSnake nextDirection model
        in
        ( { model
            | delta = model.delta - frameDuration
            , snake = newSnake
            , currentDirection = nextDirection
          }
        , cmd
        )


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


walkSnake : Direction -> Model -> ( List Point, Cmd Msg )
walkSnake dir { snake, food } =
    let
        newHead =
            walkHead dir snake
    in
    if newHead == food then
        ( newHead :: snake, newFoodCmd )

    else
        ( newHead :: walkTail snake, Cmd.none )


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
        [ board model
        , div [ class "score" ] [ model.snake |> List.length |> (+) -3 |> String.fromInt |> (++) "Score: " |> text ]
        ]


board : Model -> Html Msg
board model =
    let
        snakeElements =
            model.snake
                |> map (dot "snake")
                |> List.indexedMap (\k v -> ( String.fromInt k, v ))

        overlayDiv =
            div [ class "overlay" ] [ text model.message ]

        pausedClass =
            if model.running then
                ""

            else
                "paused"
    in
    Html.Keyed.node "div"
        [ class "board"
        , class pausedClass
        , tabindex 0
        , onKeyDown mapDirection
        , onFocus Play
        , onBlur (Pause "Paused - Click here to resume")
        ]
        (( "o", overlayDiv ) :: ( "f", dot "food" model.food ) :: snakeElements)


dot : String -> Point -> Html Msg
dot kind point =
    let
        x =
            String.fromInt (point.x * 10)

        y =
            String.fromInt (point.y * 10)

        translate =
            "translate(" ++ x ++ "px, " ++ y ++ "px)"
    in
    div [ class "dot", class kind, style "transform" translate ] []


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
            NoOp
