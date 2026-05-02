module Rules exposing (..)

import Html exposing (Html)
import Polygon exposing (..)
import Shapes exposing (..)
import String exposing (fromFloat)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, viewBox, width)
import Util exposing (..)


type alias PC =
    { poly : Polygon, col : Color, centre : Point, dist : Float, scale : Float }


collides : PC -> PC -> Bool
collides p1 p2 =
    distance p1.centre p2.centre < p1.dist + p2.dist - epsilon


type alias Rule =
    { anchor : PC
    , additions : List PC
    , rotatable : Bool
    , subdivide : Bool
    , fragment : Float
    , bounds : ( Point, Point )
    }


r : Rule
r =
    { anchor = squ
    , additions = []
    , rotatable = True
    , subdivide = False
    , fragment = 1
    , bounds = ( { x = 0, y = 0 }, { x = 1, y = 1 } )
    }


eq : PC -> PC -> Bool
eq p1 p2 =
    equals p1.poly p2.poly && (p1.col == p2.col) && (p1.scale == p2.scale)


eq2 : PC -> PC -> Bool
eq2 p1 p2 =
    (p1.poly.lengths == p2.poly.lengths) && (p1.poly.angles == p2.poly.angles) && (p1.col == p2.col) && (p1.scale == p2.scale)


applies : Rule -> PC -> Bool
applies rule p =
    if rule.rotatable then
        eq2 rule.anchor p

    else
        eq rule.anchor p


tr : Point -> PC -> PC
tr p pc =
    { pc | poly = translate p pc.poly, centre = add pc.centre p }


rt : Point -> Float -> PC -> PC
rt origin angle pc =
    { pc | poly = pc.poly |> setRotation (angle + pc.poly.rotation) |> setOrigin (rotateAround origin angle pc.poly.origin), centre = pc.centre |> rotateAround origin angle }


rto : Float -> PC -> PC
rto =
    rt { x = 0, y = 0 }


sz : Float -> PC -> PC
sz size pc =
    let
        p =
            pc.poly
    in
    { pc | poly = { p | lengths = List.map (\l -> l * size) p.lengths }, centre = mul size (sub pc.centre pc.poly.origin) |> add pc.poly.origin, dist = pc.dist * size }


pt : Float -> PC -> Point
pt alfa pc =
    let
        nextP =
            getPoint (floor alfa) (rescale pc).poly

        prevP =
            getPoint (ceiling alfa) (rescale pc).poly
    in
    add nextP (mul (toFloat (ceiling alfa) - alfa) (sub prevP nextP))


sc : Float -> PC -> PC
sc scalar pc =
    { pc | scale = pc.scale * scalar, centre = mul scalar (sub pc.centre pc.poly.origin) |> add pc.poly.origin, dist = pc.dist * scalar }


rescale : PC -> PC
rescale pc =
    let
        p =
            pc.poly
    in
    { pc | poly = { p | lengths = List.map (\l -> l * pc.scale) p.lengths } }


renderRule : Rule -> Html msg
renderRule { anchor, additions, bounds } =
    let
        ( tl1, br1 ) =
            bounds

        tl =
            sub tl1 { x = 0.4, y = 0.4 }

        br =
            add br1 { x = 0.8, y = 0.8 }
    in
    svg
        [ viewBox (fromFloat tl.x ++ " " ++ fromFloat tl.y ++ " " ++ fromFloat br.x ++ " " ++ fromFloat br.y)
        , width "200"
        , height "200"
        ]
        ((additions
            |> List.map rescale
            |> List.concatMap
                (\addition ->
                    [ polygonSvg addition.poly 1 { x = 0, y = 0 } addition.col 0.04
                    , pointSvg addition.centre 1 { x = 0, y = 0 } 0.04
                    ]
                )
         )
            ++ [ polygonSvg (rescale anchor).poly 1 { x = 0, y = 0 } anchor.col 0.08, pointSvg anchor.centre 1 { x = 0, y = 0 } 0.04 ]
        )


type alias Tess =
    { rules : List Rule
    , open : List PC
    , closed : List PC
    , size : Float
    }


renderTess : Tess -> Bool -> List (Svg msg)
renderTess tess animated =
    if animated then
        renderAnimatedTess tess

    else
        renderStaticTess tess


renderStaticTess : Tess -> List (Svg msg)
renderStaticTess { closed, size } =
    closed |> List.map rescale |> List.map (\p -> polygonSvg p.poly size { x = 0, y = 0 } p.col 2)


renderAnimatedTess : Tess -> List (Svg msg)
renderAnimatedTess { closed, size } =
    closed
        |> List.map rescale
        |> List.indexedMap
            (\i p -> polygonAnimatedSvg p.poly size { x = 0, y = 0 } p.col 2 i (List.length closed))


step : Tess -> ( Point, Point ) -> Tess
step tess bounds =
    case tess.open of
        [] ->
            tess

        -- Pick the first open polygon, check its validity and apply all rules to it
        p :: rest ->
            if not (inside p.centre bounds) || List.any (collides p) tess.closed then
                { tess
                    | open = rest
                }

            else
                let
                    new_ps =
                        tess.rules
                            |> List.filter (\rule -> applies rule p)
                            |> List.concatMap
                                (\rule ->
                                    rule.additions
                                        |> List.map
                                            (\p2 ->
                                                p2
                                                    |> rt rule.anchor.poly.origin
                                                        (p.poly.rotation
                                                            - rule.anchor.poly.rotation
                                                        )
                                                    |> tr p.poly.origin
                                                    |> tr (neg rule.anchor.poly.origin)
                                            )
                                )
                in
                { tess
                    | open = rest ++ new_ps
                    , closed = p :: tess.closed
                }


fix : Tess -> ( Point, Point ) -> Tess
fix tess bounds =
    case tess.open of
        [] ->
            { tess | closed = List.reverse tess.closed }

        _ ->
            fix (step tess bounds) bounds


squ : PC
squ =
    { poly = square, col = Primary, centre = { x = 0.5, y = 0.5 }, dist = 0.5, scale = 1 }


eqi : PC
eqi =
    { poly = equilateral, col = Primary, centre = { x = 0.5, y = sqrt 3 / 6 }, dist = sqrt 3 / 6, scale = 1 }


hex : PC
hex =
    { poly = hexagon, col = Primary, centre = { x = 0.5, y = 0.86 }, dist = 0.86, scale = 1 } |> rt { x = 0, y = 0 } 30


hexv : PC
hexv =
    { poly = hexagon, col = Primary, centre = { x = 0.5, y = 0.86 }, dist = 0.86, scale = 1 }


oct : PC
oct =
    { poly = octagon, col = Primary, centre = { x = 0.5, y = 0.5 + sqrt 0.5 }, dist = 0.5 + sqrt 0.5, scale = 1 }


dod : PC
dod =
    { poly = dodecagon, col = Primary, centre = { x = 0.5 + sqrt 3 / 2, y = 0.5 + sqrt 3 / 2 }, dist = 1 + sqrt 3 / 2, scale = 1 }
