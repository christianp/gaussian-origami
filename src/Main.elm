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
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Events as Events
import Json.Decode as Decode
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import Set
import Svg.Keyed
import Markdown


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

type alias Rect =
 { x1 : Int
 , y1 : Int
 , x2 : Int
 , y2 : Int
 }

type LineStyle
 = Normal
 | Selecting
 | Deselecting
 | Selected

type Direction
 = Forwards
 | Backwards

type Line
 = Horizontal Int -- crosses y axis at Int
 | Vertical Int -- crosses x axis at Int
 | UpDiagonal Int -- positive gradient, crosses y axis at Int
 | DownDiagonal Int -- negative gradient, crosses y axis at Int

type Command
 = Fold
 | Unfold

type alias Move = (Command,Direction,Line)

type PointStatus
 = FixedPoint
 | Disappearing
 | Appearing

type alias Step = (List Point, Move)

type alias Model =
  { points : List Point
  , command : Command
  , mouse : (Float, Float)
  , scale: Int
  , selectedLine: Maybe Line
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
          , selectedLine = Nothing
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

    Click -> do_click model
      
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
    
do_click : Model -> (Model, Cmd Msg)
do_click model = 
    let
        (dir,line) = closest_line model.mouse
        cmd = make_command model
    in
        case model.selectedLine of
            Nothing -> ( {model | selectedLine = Just line}, Cmd.none)
            Just l -> if line==l then ({model | selectedLine = Nothing}, Cmd.none) else (setUrl <| do_move model l)

do_move : Model -> Line -> Model
do_move model line =
    let
        dir = side_of line model.mouse
        cmd = (model.command, dir, line)
        npoints = do_command cmd model.points
    in
        if npoints==model.points then 
            {model | selectedLine = Nothing}
        else
            {model | points = npoints, previous = (model.points,cmd)::model.previous, next = [], selectedLine = Nothing }

make_command : Model -> Maybe Move
make_command model = Maybe.map (\l -> (model.command, side_of l model.mouse, l)) model.selectedLine
            

do_command (command,dir,line) points =
    let
        cmd = case command of
            Fold -> \p -> [fold (dir,line) p]
            Unfold -> unfold (dir,line)
    in
        Set.toList <| Set.fromList <| List.concatMap cmd points

do_commands : List Move -> List Point -> (List Point, List Step)
do_commands commands points =
    let
        f cmd (opoints,history) = 
            let
                npoints = do_command cmd opoints
            in
                (npoints,(opoints,cmd)::history)
    in
        List.foldl f (points,[]) commands

closest_horizontal_line : Float -> Float -> ((Direction,Line), Float)
closest_horizontal_line x y =
    let
        iy = round y
        d = abs (y - (toFloat iy))
        dir = if x>=0 then Forwards else Backwards
    in
        ((dir,Horizontal iy), d)

closest_vertical_line : Float -> Float -> ((Direction,Line), Float)
closest_vertical_line x y =
    let
        ix = round x
        d = abs (x - (toFloat ix))
        dir = if y>=0 then Forwards else Backwards
    in
        ((dir,Vertical ix), d)
        
s2 = 1.0 / (sqrt 2)

rotate45 x y = (s2 * (y + x), s2 * (y - x))

closest_downdiagonal_line : Float -> Float -> ((Direction,Line), Float)
closest_downdiagonal_line x y =
    let
        (rx,ry) = rotate45 x y
        (l,d) = closest_horizontal_line (rx/s2/2) (ry/s2/2)
        dir = if x >= -y then Forwards else Backwards
    in
        case l of
            (_,Horizontal z) -> ((dir,DownDiagonal z), d)
            _ -> ((Forwards,UpDiagonal 0), 2)

closest_updiagonal_line : Float -> Float -> ((Direction,Line), Float)
closest_updiagonal_line x y =
    let
        (rx,ry) = rotate45 x y
        (l,d) = closest_vertical_line (rx/s2/2) (ry/s2/2)
        dir = if x >= y then Forwards else Backwards
    in
        case l of
            (_,Vertical z) -> ((dir,UpDiagonal z), d)
            _ -> ((Forwards,UpDiagonal 0), 2)

lowest default values = case values of
    [] -> default
    (v,k)::rest -> 
        let
            (lv,lk) = lowest default rest
        in
            if lk<k then (lv,lk) else (v,k)
        
closest_line : (Float,Float) -> (Direction,Line)
closest_line (x,y) =
    let
        lines = [ closest_horizontal_line x y
                , closest_vertical_line x y
                , closest_updiagonal_line x y
                , closest_downdiagonal_line x y
                ]
    in
        first <| lowest ((Forwards,Horizontal 0), 2.0) lines

side_of : Line -> (Float,Float) -> Direction
side_of line (x,y) = 
    let 
        left = case line of
            Horizontal z -> y > (toFloat z)
            Vertical z -> x > (toFloat z)
            UpDiagonal z -> x > (-y + 2*(toFloat z))
            DownDiagonal z -> x > (y - 2*(toFloat z))
    in
        if left then Backwards else Forwards

fold : (Direction,Line) -> Point -> Point
fold (dir,line) p = 
    let
        (px,py) = p
        rp = reflect line p
    in
        case line of
            Horizontal y ->
                case dir of
                    Forwards -> if py > y then rp else p
                    Backwards -> if py < y then rp else p
            Vertical x ->
                case dir of
                    Forwards -> if px > x then rp else p
                    Backwards -> if px < x then rp else p
            DownDiagonal z -> 
                case dir of
                    Forwards -> if px > py-2*z then rp else p
                    Backwards -> if px < py-2*z then rp else p
            UpDiagonal z -> 
                case dir of
                    Forwards -> if px > 2*z-py then rp else p
                    Backwards -> if px < 2*z-py then rp else p
                    
unfold (dir,line) p = [p, fold (dir,line) p]

reflect : Line -> Point -> Point
reflect line (px,py) = case line of
    Horizontal y -> (px,2*y - py)
    Vertical x -> (2*x - px,py)
    DownDiagonal z -> (py-2*z, px+2*z)
    UpDiagonal z -> (-py+2*z, 2*z-px)


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
      [ div [HA.id "controls"] 
        [ button [HA.id "command", Html.Events.onClick Toggle] [ Html.text <| describeCommand model.command ]
        , button [HA.id "zoom-in", Html.Events.onClick ZoomIn] [ Html.text "Zoom in" ]
        , button [HA.id "zoom-out", Html.Events.onClick ZoomOut] [ Html.text "Zoom out" ]
        , button [HA.id" undo", Html.Events.onClick Undo] [ Html.text "Undo" ]
        , button [HA.id "redo", Html.Events.onClick Redo] [ Html.text "Redo" ]
        , div [HA.id "steps"] [Html.text <| show_steps model]
        ]
      , viewSVG model
      , viewRules
      ]
  }
  
describeCommand cmd = case cmd of
    Fold -> "Folding"
    Unfold -> "Unfolding"

step_separator = "!"

parseQuery : Url -> List Move
parseQuery url = ((QP.string "steps" |> QP.map (Maybe.map <| String.split step_separator) |> query |> parse) {url | path=""}) |> Maybe.withDefault (Just []) >> Maybe.map parseSteps >> Maybe.map (Maybe.withDefault []) >> Maybe.withDefault []

parseSteps : List String -> Maybe (List Move)
parseSteps = List.map parseStep >> maybeAll

maybeAll : List (Maybe a) -> Maybe (List a)
maybeAll l = case l of
    [] -> Just []
    a::rest -> case a of
        Just x -> maybeAll rest |> Maybe.andThen (\r -> Just (x::r))
        Nothing -> Nothing

parseStep : String -> Maybe Move
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
        Maybe.map4 (\c -> \o -> \d -> \i -> (c,d,o i)) cmd orientation direction z


buildQuery : Model -> String
buildQuery model = List.map (second >> buildStep) model.previous |> List.reverse |> String.join step_separator |> (\s -> toQuery [UB.string "steps" s]) |> String.dropLeft 1
        
buildStep (cmd,dir,line) =
    let
        c = case cmd of
            Fold -> "f"
            Unfold -> "u"
        (o,zz,dd) = case line of
            Horizontal z -> ("h",z,dir)
            Vertical z -> ("v",z,dir)
            UpDiagonal z -> ("u",z,dir)
            DownDiagonal z -> ("d",z,dir)
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
        nextpoints = Maybe.map (\cmd -> do_command cmd model.points) (make_command model) |> Maybe.map Set.fromList
        newpoints = case nextpoints of
            Nothing -> []
            Just ps -> Set.toList <| Set.diff ps pointset
        disappearingpoints = case nextpoints of
            Nothing -> []
            Just ps -> Set.toList <| Set.diff pointset ps
        fixedpoints = case nextpoints of
            Nothing -> model.points
            Just ps -> Set.toList <| Set.intersect pointset ps
        all_points : List (PointStatus,Point)
        all_points = 
               (List.map (\p -> (FixedPoint,p)) fixedpoints)
            ++ (List.map (\p -> (Disappearing,p)) disappearingpoints)
            ++ (List.map (\p -> (Appearing,p)) newpoints)
        scale = model.scale
        vb = String.join " " <| List.map String.fromInt [-scale, -scale, 2*scale, 2*scale]
        view_selected_line = case model.selectedLine of
            Nothing -> []
            Just l -> [view_line Selected l]
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
               List.concatMap ((\z -> [Horizontal z,Vertical z,UpDiagonal z,DownDiagonal z]) >> List.map (view_line Normal)) (List.range -draw_bound draw_bound)
            ++ [view_closest_line model]
            ++ [view_points model all_points]
            )
    
view_closest_line model =
    let
        (dir,line) = closest_line model.mouse
        deselecting = model.selectedLine == Just (closest_line model.mouse |> second)
        style = if deselecting then Deselecting else Selecting
        closest = view_line style line
    in
        case model.selectedLine of
            Nothing -> closest
            Just sl -> if sl==line then closest else view_line Selected sl
        
would_disappear model p =
    let
        line = closest_line model.mouse
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

line_width = 0.05

view_line style line = 
    let
        ((x1i,y1i),(x2i,y2i)) = case line of
            Horizontal y -> view_horizontal_line y
            Vertical x -> view_vertical_line x
            UpDiagonal n -> view_updiagonal_line n
            DownDiagonal n -> view_downdiagonal_line n
        style_attrs = case style of
            Normal -> [ stroke "hsla(0,0%,0%,0.2)"
                      , strokeWidth <| String.fromFloat line_width
                      ]
            Deselecting -> [ stroke "red"
                           , strokeWidth <| String.fromFloat (2*line_width)
                           ]
            _ -> [ stroke "hsl(240,50%,50%)"
                 , strokeWidth <| String.fromFloat (1.5*line_width)
                 ]
    in 
        Svg.line (style_attrs ++ [x1 <| String.fromInt x1i, x2 <| String.fromInt x2i, y1 <| String.fromInt y1i, y2 <| String.fromInt y2i]) []

view_vertical_line : Int -> (Point,Point)
view_vertical_line x = ((x,-draw_bound),(x,draw_bound))

view_horizontal_line : Int -> (Point,Point)
view_horizontal_line x = ((-draw_bound,x),(draw_bound,x))

view_updiagonal_line : Int -> (Point,Point)
view_updiagonal_line x = ((-draw_bound,2*x+draw_bound),(draw_bound,2*x-draw_bound))

view_downdiagonal_line : Int -> (Point,Point)
view_downdiagonal_line x = ((-draw_bound,2*x-draw_bound),(draw_bound,2*x+draw_bound))

para content = Html.p [] [Html.text content]

viewRules : Html Msg
viewRules = Markdown.toHtml [] """
# Gaussian origami

There are two things you can do:

* *Fold* - remove every point on one side of a line, and add their mirror images on the other side.
* *Unfold* - keep all the existing points, and add the mirror images of the points on one side of the line.

To fold or unfold, click a line to select it, then click on the side of the line that you want to fold/unfold to.

What patterns can you make?
"""
