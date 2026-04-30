module Util exposing (..)


type Color
    = Primary
    | Secondary
    | Ternary
    | Quart
    | Stroke


getVar : Color -> String
getVar color =
    case color of
        Primary ->
            "--primary-color"

        Secondary ->
            "--secondary-color"

        Ternary ->
            "--ternary-color"

        Quart ->
            "--quart-color"

        Stroke ->
            "--stroke-color"


type alias Point =
    { x : Float, y : Float }


add : Point -> Point -> Point
add p q =
    { x = p.x + q.x, y = p.y + q.y }


sub : Point -> Point -> Point
sub p q =
    { x = p.x - q.x, y = p.y - q.y }


mul : Float -> Point -> Point
mul s p =
    { x = s * p.x, y = s * p.y }


mulx : Float -> Point -> Point
mulx s p =
    { x = s * p.x, y = p.y }


muly : Float -> Point -> Point
muly s p =
    { x = p.x, y = s * p.y }


neg : Point -> Point
neg { x, y } =
    { x = -x, y = -y }


mix2Color : Int -> Color
mix2Color n =
    case modBy 2 n of
        0 ->
            Primary

        _ ->
            Secondary


mix3Color : Int -> Color
mix3Color n =
    case modBy 3 n of
        0 ->
            Primary

        1 ->
            Secondary

        _ ->
            Ternary


mix4Color : Int -> Color
mix4Color n =
    case modBy 4 n of
        0 ->
            Primary

        1 ->
            Secondary

        2 ->
            Ternary

        _ ->
            Quart


tripleOp : (a -> b) -> ( a, a, a ) -> ( b, b, b )
tripleOp f ( x, y, z ) =
    ( f x, f y, f z )


zip3 : ( a, a, a ) -> ( b, b, b ) -> ( ( a, b ), ( a, b ), ( a, b ) )
zip3 ( a1, a2, a3 ) ( b1, b2, b3 ) =
    ( ( a1, b1 ), ( a2, b2 ), ( a3, b3 ) )


inside : Point -> ( Point, Point ) -> Bool
inside p1 ( p2, p3 ) =
    p1.x >= p2.x && p1.x <= p3.x && p1.y >= p2.y && p1.y <= p3.y


distance : Point -> Point -> Float
distance p1 p2 =
    let
        dy =
            p2.y - p1.y

        dx =
            p2.x - p1.x
    in
    sqrt (dy * dy + dx * dx)


rotateAround : Point -> Float -> Point -> Point
rotateAround origin angle p =
    let
        p2 =
            sub p origin

        cosa =
            -angle |> degrees |> cos

        sina =
            -angle |> degrees |> sin

        p3 =
            { x = p2.x * cosa - p2.y * sina, y = p2.y * cosa + p2.x * sina }
    in
    add p3 origin


epsilon : Float
epsilon =
    0.001
