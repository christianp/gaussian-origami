module Main exposing (..)
-- Show an analog clock for your time zone.
--
-- Dependencies:
--   elm install elm/svg
--   elm install elm/time
--
-- For a simpler version, check out:
--   https://elm-lang.org/examples/time
--

import Browser exposing (Document)
import Browser.Navigation as Navigation exposing (Key, replaceUrl)
import Dict
import Url exposing (Url)
import Url.Builder as UB exposing (toQuery)
import Url.Parser exposing (Parser, parse, query)
import Url.Parser.Query as QP
import Html exposing (Html, div, button)
import Html.Events
import Svg exposing (..)
import Svg.Events as Events
import Json.Decode as Decode
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import Set
import Svg.Keyed


-- MAIN


main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = ChangeUrlRequest
    , onUrlChange = UrlChanging
    }



-- MODEL

type alias Point = (Int,Int)

type Direction
 = Forwards
 | Backwards

type Line
 = Horizontal Int Direction -- crosses y axis at Int
 | Vertical Int Direction -- crosses x axis at Int
 | UpDiagonal Int Direction -- positive gradient, crosses y axis at Int
 | DownDiagonal Int Direction -- negative gradient, crosses y axis at Int

type Command
 = Fold
 | Unfold

type PointStatus
 = FixedPoint
 | Disappearing
 | Appearing

type alias Step = (List Point, (Command, Line))

type alias Model =
  { points : List Point
  , command : Command
  , mouse : (Float, Float)
  , scale: Int
  , previous : List Step
  , next: List Step
  , url : Url
  , locationKey : Key
  }

init : () -> Url -> Key -> (Model, Cmd Msg)
init _ url key =
    let
        steps = parseQuery url
        initpoints = [(0,0)]
        (points,previous) = do_commands steps initpoints
    in
        ( { points = points
          , command = Unfold
          , mouse = (0.0, 0.0)
          , scale = 5
          , previous = previous
          , next = []
          , url = url
          , locationKey = key
          }
          , Cmd.none
          )


opposite : Command -> Command
opposite cmd = case cmd of
  Fold -> Unfold
  Unfold -> Fold


-- UPDATE


type Msg
  = Toggle
  | Click
  | MouseMoveAt Float Float
  | ZoomIn
  | ZoomOut
  | Undo
  | Redo
  | ChangeUrlRequest Browser.UrlRequest
  | UrlChanging Url

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Toggle ->
      ( { model | command = opposite model.command }
      , Cmd.none
      )

    Click -> setUrl <| do_click model
      
    MouseMoveAt x y -> 
      ( { model | mouse = (x,y)}
      , Cmd.none
      )
    
    ZoomIn ->
      ( { model | scale = Basics.max 2 (model.scale-1) }
      , Cmd.none
      )
      
    ZoomOut ->
      ( { model | scale = model.scale+1 }
      , Cmd.none
      )
      
    Undo -> setUrl <| undo model
    Redo -> setUrl <| redo model

    ChangeUrlRequest _ -> (model, Cmd.none)
    UrlChanging _ -> (model, Cmd.none)

setUrl model =
    let
        url = model.url
        nurl = {url | query = Just <| buildQuery model}
    in
        (model,replaceUrl model.locationKey (Url.toString nurl))

undo model = case model.previous of
    [] -> model
    (p,cmd)::rest -> { model | points = p, previous = rest, next = (model.points,cmd)::model.next }

redo model = case model.next of
    [] -> model
    (p,cmd)::rest -> { model | points = p, next = rest, previous = (model.points,cmd)::model.previous }
    
do_click : Model -> Model
do_click model = 
    let
        (mx,my) = model.mouse
        line = closest_line mx my
        cmd = (make_command model)
    in
        {model | points = do_command cmd model.points, previous = (model.points,cmd)::model.previous, next = [] }

make_command model = 
    let
        (mx,my) = model.mouse
        line = closest_line mx my
    in
        (model.command, line)
        

do_command (command,line) points =
    let
        cmd = case command of
            Fold -> \p -> [fold line p]
            Unfold -> unfold line
    in
        Set.toList <| Set.fromList <| List.concatMap cmd points

do_commands commands points =
    let
        f cmd (opoints,history) = 
            let
                npoints = do_command cmd opoints
            in
                (npoints,(opoints,cmd)::history)
    in
        List.foldl f (points,[]) commands

closest_horizontal_line : Float -> Float -> (Line, Float)
closest_horizontal_line x y =
    let
        iy = round y
        d = abs (y - (toFloat iy))
        dir = if x>=0 then Forwards else Backwards
    in
        (Horizontal iy dir, d)

closest_vertical_line : Float -> Float -> (Line, Float)
closest_vertical_line x y =
    let
        ix = round x
        d = abs (x - (toFloat ix))
        dir = if y>=0 then Forwards else Backwards
    in
        (Vertical ix dir, d)
        
s2 = 1.0 / (sqrt 2)

rotate45 x y = (s2 * (y + x), s2 * (y - x))

closest_downdiagonal_line : Float -> Float -> (Line, Float)
closest_downdiagonal_line x y =
    let
        (rx,ry) = rotate45 x y
        (l,d) = closest_horizontal_line (rx/s2/2) (ry/s2/2)
        dir = if x >= -y then Forwards else Backwards
    in
        case l of
            Horizontal z _ -> (DownDiagonal z dir, d)
            _ -> (UpDiagonal 0 Forwards, 2)

closest_updiagonal_line : Float -> Float -> (Line, Float)
closest_updiagonal_line x y =
    let
        (rx,ry) = rotate45 x y
        (l,d) = closest_vertical_line (rx/s2/2) (ry/s2/2)
        dir = if x >= y then Forwards else Backwards
    in
        case l of
            Vertical z _ -> (UpDiagonal z dir, d)
            _ -> (UpDiagonal 0 Forwards, 2)

lowest default values = case values of
    [] -> default
    (v,k)::rest -> 
        let
            (lv,lk) = lowest default rest
        in
            if lk<k then (lv,lk) else (v,k)
        
closest_line : Float -> Float -> Line
closest_line x y =
    let
        lines = [ closest_horizontal_line x y
                , closest_vertical_line x y
                , closest_updiagonal_line x y
                , closest_downdiagonal_line x y
                ]
    in
        first <| lowest (Horizontal 0 Forwards, 2.0) lines

fold : Line -> Point -> Point
fold line p = 
    let
        (px,py) = p
        rp = reflect line p
    in
        case line of
            Horizontal y dir ->
                case dir of
                    Forwards -> if py > y then rp else p
                    Backwards -> if py < y then rp else p
            Vertical x dir ->
                case dir of
                    Forwards -> if px > x then rp else p
                    Backwards -> if px < x then rp else p
            DownDiagonal z dir -> 
                case dir of
                    Forwards -> if px > py-2*z then rp else p
                    Backwards -> if px < py-2*z then rp else p
            UpDiagonal z dir -> 
                case dir of
                    Forwards -> if px > 2*z-py then rp else p
                    Backwards -> if px < 2*z-py then rp else p
                    
unfold line p = [p, fold line p]

reflect : Line -> Point -> Point
reflect line (px,py) = case line of
    Horizontal y dir -> (px,2*y - py)
    Vertical x dir -> (2*x - px,py)
    DownDiagonal z dir -> (py-2*z, px+2*z)
    UpDiagonal z dir -> (-py+2*z, 2*z-px)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

cartesian_product l1 l2 = List.foldl (++) [] (List.map (\a -> List.map (\b -> (a,b)) l2) l2)


view : Model -> Document Msg

view model = 
  { title = "Gaussian Origami"
  , body =
      [ div [] 
        [ button [ Html.Events.onClick Toggle] [ Html.text <| describeCommand model.command ]
        , button [ Html.Events.onClick ZoomIn] [ Html.text "Zoom in" ]
        , button [ Html.Events.onClick ZoomOut] [ Html.text "Zoom out" ]
        , button [ Html.Events.onClick Undo] [ Html.text "Undo" ]
        , button [ Html.Events.onClick Redo] [ Html.text "Redo" ]
        , Html.text <| show_steps model
--        , Html.text <| debugstring model
        ]
      , viewSVG model
      ]
  }
  
describeCommand cmd = case cmd of
    Fold -> "Folding"
    Unfold -> "Unfolding"

step_separator = "!"

parseQuery : Url -> List (Command,Line)
parseQuery url = ((QP.string "steps" |> QP.map (Maybe.map <| String.split step_separator) |> query |> parse) {url | path=""}) |> Maybe.withDefault (Just []) >> Maybe.map parseSteps >> Maybe.map (Maybe.withDefault []) >> Maybe.withDefault []

parseSteps : List String -> Maybe (List (Command, Line))
parseSteps = List.map parseStep >> maybeAll

maybeAll : List (Maybe a) -> Maybe (List a)
maybeAll l = case l of
    [] -> Just []
    a::rest -> case a of
        Just x -> maybeAll rest |> Maybe.andThen (\r -> Just (x::r))
        Nothing -> Nothing

parseStep : String -> Maybe (Command, Line)
parseStep s = 
    let
        cmds = Dict.fromList
            [ ("f",Fold)
            , ("u",Unfold)
            ]
        cmd = Dict.get (String.slice 0 1 s) cmds
        orientations = Dict.fromList
            [ ("h",Horizontal)
            , ("v",Vertical)
            , ("u",UpDiagonal)
            , ("d",DownDiagonal)
            ]
        orientation = Dict.get (String.slice 1 2 s) orientations
        directions = Dict.fromList
            [ ("f",Forwards)
            , ("b",Backwards)
            ]
        direction = Dict.get (String.slice 2 3 s) directions
        z = String.dropLeft 3 s |> String.toInt
    in
        Maybe.map4 (\c -> \o -> \d -> \i -> (c,o i d)) cmd orientation direction z


buildQuery : Model -> String
buildQuery model = List.map (second >> buildStep) model.previous |> List.reverse |> String.join step_separator |> (\s -> toQuery [UB.string "steps" s]) |> String.dropLeft 1
        
buildStep (cmd,line) =
    let
        c = case cmd of
            Fold -> "f"
            Unfold -> "u"
        (o,zz,dd) = case line of
            Horizontal z dir -> ("h",z,dir)
            Vertical z dir -> ("v",z,dir)
            UpDiagonal z dir -> ("u",z,dir)
            DownDiagonal z dir -> ("d",z,dir)
        i = String.fromInt zz
        d = case dd of
            Forwards -> "f"
            Backwards -> "b"
    in
        c++o++d++i

show_steps model =
    let
        n = List.length model.previous
    in
        (String.fromInt n)++" "++(if n==1 then "step" else "steps")

debugstring model =
    let
        steps = buildQuery model
    in
        Debug.toString steps

draw_bound = 100

viewSVG model =
    let
        pointset = Set.fromList model.points
        nextpoints = Set.fromList <| do_command (make_command model) model.points
        newpoints = Set.toList <| Set.diff nextpoints pointset
        disappearingpoints = Set.toList <| Set.diff pointset nextpoints
        fixedpoints = Set.toList <| Set.intersect pointset nextpoints
        all_points : List (PointStatus,Point)
        all_points = 
               (List.map (\p -> (FixedPoint,p)) fixedpoints)
            ++ (List.map (\p -> (Disappearing,p)) disappearingpoints)
            ++ (List.map (\p -> (Appearing,p)) newpoints)
        scale = model.scale
        vb = String.join " " <| List.map String.fromInt [-scale, -scale, 2*scale, 2*scale]
    in
        svg
            [ viewBox vb
            , width "800"
            , height "800"
            , Events.onClick Click 
            , Events.on "svgclick" 
                    <| Decode.map2 MouseMoveAt
                        (Decode.at ["detail", "x"] Decode.float)
                        (Decode.at ["detail", "y"] Decode.float) 
            ]
            (
               List.map view_vertical_line (List.range -draw_bound draw_bound)
            ++ List.map view_horizontal_line (List.range -draw_bound draw_bound)
            ++ List.map view_updiagonal_line (List.range -draw_bound draw_bound)
            ++ List.map view_downdiagonal_line (List.range -draw_bound draw_bound)
            ++ [view_closest_line model]
            ++ [view_points model all_points]
            )
    
view_closest_line model =
    let
        (x,y) = model.mouse
        line = closest_line x y
    in
        view_line line
        
would_disappear model p =
    let
        (mx,my) = model.mouse
        line = closest_line mx my
        rp = fold line p
    in
        case model.command of
            Fold -> rp /= p
            Unfold -> False

view_points model points = Svg.Keyed.node "g" [] (List.map (\(s,p) -> view_point model s p) points)

view_point : Model -> PointStatus -> Point -> (String, Svg Msg)
view_point model status p = 
    let
        (x,y) = p
        radius = case status of
            FixedPoint -> "0.25"
            Appearing -> "0.15"
            Disappearing -> "0.3"
        fillStyle = case status of
            FixedPoint -> "green"
            Disappearing -> "gray"
            Appearing -> "blue"
        key = (String.fromInt x)++","++(String.fromInt y)
    in
        ( key
        , circle 
            [ cx <| String.fromInt x
            , cy <| String.fromInt y
            , r radius
            , fill fillStyle
            ] []
        )

line_width = "0.05"

view_line line = case line of
    Horizontal y d -> view_horizontal_line y
    Vertical x d -> view_vertical_line x
    UpDiagonal n d -> view_updiagonal_line n
    DownDiagonal n d -> view_downdiagonal_line n

view_vertical_line : Int -> Svg Msg
view_vertical_line x = line 
  [ x1 <| String.fromInt x
  , x2 <| String.fromInt x
  , y1 <| String.fromInt (-draw_bound)
  , y2 <| String.fromInt (draw_bound)
  , stroke "hsla(0,0%,0%,0.2)"
  , strokeWidth line_width
  ] []

view_horizontal_line : Int -> Svg Msg
view_horizontal_line x = line 
  [ y1 <| String.fromInt x
  , y2 <| String.fromInt x
  , x1 <| String.fromInt (-draw_bound)
  , x2 <| String.fromInt (draw_bound)
  , stroke "hsla(0,0%,0%,0.2)"
  , strokeWidth line_width
  ] []

view_updiagonal_line : Int -> Svg Msg
view_updiagonal_line x = line 
  [ x1 <| String.fromInt (-draw_bound)
  , x2 <| String.fromInt (draw_bound)
  , y1 <| String.fromInt (2*x+draw_bound)
  , y2 <| String.fromInt (2*x-draw_bound)
  , stroke "hsla(0,0%,0%,0.2)"
  , strokeWidth line_width
  ] []

view_downdiagonal_line : Int -> Svg Msg
view_downdiagonal_line x = line 
  [
    x1 <| String.fromInt (-draw_bound)
  , x2 <| String.fromInt (draw_bound)
  , y1 <| String.fromInt (2*x-draw_bound)
  , y2 <| String.fromInt (2*x+draw_bound)
  , stroke "hsla(0,0%,0%,0.2)"
  , strokeWidth line_width
  ] []
