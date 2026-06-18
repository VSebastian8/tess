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
    , bounds = ( { x = -5, y = -5 }, { x = 10, y = 10 } )
    }


eq : PC -> PC -> Bool
eq p1 p2 =
    equals p1.poly p2.poly && (p1.col == p2.col)


eq2 : PC -> PC -> Bool
eq2 p1 p2 =
    (p1.poly.lengths == p2.poly.lengths) && (p1.poly.angles == p2.poly.angles) && (p1.col == p2.col)


applies : Rule -> PC -> Bool
applies rule p =
    (if rule.subdivide then
        True

     else
        rule.anchor.scale == p.scale
    )
        && (if rule.rotatable then
                eq2 rule.anchor p

            else
                eq rule.anchor p
           )


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
    let
        p =
            pc.poly
    in
    { pc | poly = { p | origin = mul scalar p.origin }, scale = pc.scale * scalar, centre = mul scalar (sub pc.centre p.origin) |> add (mul scalar p.origin), dist = pc.dist * scalar }


st : Int -> PC -> PC
st n pc =
    let
        alfa =
            List.take n pc.poly.angles |> List.foldl (+) 0
    in
    pc |> tr (pc |> pt (toFloat n) |> neg) |> rto -alfa |> rto (180 * toFloat (modBy 2 n))


rescale : PC -> PC
rescale pc =
    let
        p =
            pc.poly
    in
    { pc | poly = { p | lengths = List.map (\l -> l * pc.scale) p.lengths } }


renderRule : Rule -> Html msg
renderRule { anchor, additions, bounds, subdivide } =
    let
        ( tl1, br1 ) =
            bounds

        tl =
            sub tl1 { x = 0.4, y = 0.4 }

        br =
            add br1 { x = 0.8, y = 0.8 }

        anchorSvgs =
            [ polygonSvg (rescale anchor).poly 1 { x = 0, y = 0 } anchor.col 0.08, pointSvg anchor.centre 1 { x = 0, y = 0 } 0.04 ]

        extraAnchor =
            strokePolygonSvg (rescale anchor).poly 1 { x = 0, y = 0 } 0.08

        addSvgs =
            additions
                |> List.map rescale
                |> List.concatMap
                    (\addition ->
                        [ polygonSvg addition.poly 1 { x = 0, y = 0 } addition.col 0.04
                        , pointSvg addition.centre 1 { x = 0, y = 0 } 0.04
                        ]
                    )
    in
    svg
        [ viewBox (fromFloat tl.x ++ " " ++ fromFloat tl.y ++ " " ++ fromFloat br.x ++ " " ++ fromFloat br.y)
        , width "200"
        , height "200"
        ]
        (if subdivide then
            anchorSvgs ++ addSvgs ++ [ extraAnchor ]

         else
            addSvgs ++ anchorSvgs
        )


type alias Tess =
    { rules : List Rule
    , open : List PC
    , closed : List PC
    , size : Float
    , start : Point
    }


debugTess : Tess -> Bool -> List (Svg msg)
debugTess { open, closed, size } _ =
    (Debug.log "closed: " closed |> List.map rescale |> List.map (\p -> debugPolygonSvg p.poly size { x = 0, y = 0 } p.col 2))
        ++ (Debug.log "open: " open |> List.map rescale |> List.map (\p -> polygonSvg p.poly size { x = 0, y = 0 } p.col 2))


renderTess : Tess -> Bool -> List (Svg msg)
renderTess tess animated =
    if animated then
        renderAnimatedTess tess

    else
        renderStaticTess tess


renderStaticTess : Tess -> List (Svg msg)
renderStaticTess { closed, size } =
    closed |> List.map rescale |> List.map (\p -> polygonSvg p.poly size { x = 0, y = 0 } p.col 2)



-- Debug centres
-- ++ (closed
--         |> List.map rescale
--         |> List.map
--             (\p ->
--                 pointSvg (mul size p.centre) 1 { x = 0, y = 0 } 2
--             )
--    )


renderAnimatedTess : Tess -> List (Svg msg)
renderAnimatedTess { closed, size } =
    closed
        |> List.map rescale
        |> List.indexedMap
            (\i p -> polygonAnimatedSvg p.poly size { x = 0, y = 0 } p.col 2 i (List.length closed))


modify : Tess -> PC -> List PC -> Tess
modify tess p rest =
    let
        adds =
            tess.rules
                |> List.filter (\rule -> not rule.subdivide)
                |> List.filter (\rule -> applies rule p)
                |> List.concatMap
                    (\rule ->
                        rule.additions
                            |> List.map
                                (\p2 ->
                                    p2
                                        |> rt rule.anchor.poly.origin
                                            (p.poly.rotation - rule.anchor.poly.rotation)
                                        |> tr p.poly.origin
                                        |> tr (neg rule.anchor.poly.origin)
                                )
                    )

        subs =
            tess.rules
                |> List.filter (\rule -> rule.subdivide)
                |> List.filter (\rule -> rule.fragment <= p.scale)
                |> List.filter (\rule -> applies rule p)
                |> List.concatMap
                    (\rule ->
                        rule.additions
                            |> List.map
                                (\p2 ->
                                    p2
                                        |> sc (p.scale / rule.anchor.scale)
                                        |> rt rule.anchor.poly.origin
                                            (p.poly.rotation - rule.anchor.poly.rotation)
                                        |> tr p.poly.origin
                                        |> tr (neg rule.anchor.poly.origin)
                                )
                    )
    in
    { tess
        | open = subs ++ rest ++ adds
        , closed =
            if List.isEmpty subs then
                p :: tess.closed

            else
                tess.closed
    }


hugePoly : PC -> ( Point, Point ) -> Bool
hugePoly p ( b1, b2 ) =
    distance b2 b1 < p.scale


step : Tess -> ( Point, Point ) -> Tess
step tess bounds =
    case tess.open of
        [] ->
            tess

        -- Pick the first open polygon, check its validity and apply all rules to it
        p :: rest ->
            if not (inside p.centre bounds) || List.any (collides p) tess.closed || hugePoly p bounds then
                { tess | open = rest }

            else
                modify tess p rest


stepN : Tess -> ( Point, Point ) -> Int -> Tess
stepN tess bounds n =
    if List.isEmpty tess.open || n == 0 then
        { tess | closed = List.reverse tess.closed }

    else
        stepN (step tess bounds) bounds (n - 1)


fix : Tess -> ( Point, Point ) -> Tess
fix tess bounds =
    case tess.open of
        [] ->
            { tess | closed = List.reverse tess.closed }

        _ ->
            fix (step tess bounds) bounds


keep0 : Float -> a -> a -> a
keep0 val x y =
    if val == 0 then
        x

    else
        y


placeStart : Tess -> Float -> Float -> Tess
placeStart tess w h =
    let
        centre =
            case tess.rules |> List.head of
                Nothing ->
                    { x = 0, y = 0 }

                Just rule ->
                    rule.anchor.centre

        distance =
            { x = keep0 tess.start.x 0 (tess.start.x * w / tess.size - centre.x)
            , y = keep0 tess.start.y 0 (tess.start.y * h / tess.size - centre.y)
            }
    in
    { tess
        | open = tess.open |> List.map (tr distance)
        , closed = tess.closed |> List.map (tr distance)
    }


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


iso : PC
iso =
    { poly = isosceles, col = Primary, centre = { x = 1 / 3, y = 1 / 3 }, dist = 1 / 6, scale = 1 }


ois : PC
ois =
    { poly = obtuseIso, col = Primary, centre = mul (2 / 3) { x = 0.25, y = cos (degrees 30) / 2 }, dist = mag { x = 0.25, y = cos (degrees 30) / 2 } / 3, scale = 1 }


rho : PC
rho =
    { poly = rhombus, col = Primary, centre = { x = cos (degrees 60) / 2, y = cos (degrees 30) / 2 }, dist = cos (degrees 30) * cos (degrees 30) / 2, scale = 1 }


rgt : PC
rgt =
    let
        c =
            add (right |> getPoint 1) (sub (right |> getPoint 2) (right |> getPoint 1) |> mul 0.5) |> mul (2 / 3)
    in
    { poly = right, col = Primary, centre = c, dist = 0.12, scale = 1 }


lft : PC
lft =
    let
        c =
            add (left |> getPoint 1) (sub (left |> getPoint 2) (left |> getPoint 1) |> mul 0.5) |> mul (2 / 3)
    in
    { poly = left, col = Primary, centre = c, dist = 0.12, scale = 1 }


kit : PC
kit =
    let
        c =
            add (sub (kite |> getPoint 3) (kite |> getPoint 1) |> mul 0.5) (kite |> getPoint 1)
    in
    { poly = kite, col = Primary, centre = c, dist = 0.21, scale = 1 }


car : PC
car =
    { poly = cairo, col = Primary, centre = { x = sqrt 2 * cos (degrees 75), y = (cos (degrees 30) + sin (degrees 30)) / 2 }, dist = 0.5, scale = 1 }


flr : PC
flr =
    { poly = floret, col = Primary, centre = { x = 1 + cos (degrees 60), y = sin (degrees 60) * (1 + 2 * cos (degrees 60)) / 2 }, dist = 0.5, scale = 1 }


pri : PC
pri =
    { poly = prism, col = Primary, centre = { x = 0.5, y = 0.5 }, dist = 0.5, scale = 1 }
