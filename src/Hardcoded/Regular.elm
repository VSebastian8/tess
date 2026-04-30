module Hardcoded.Regular exposing (hexagonalTiling, squareTiling, triangularTiling)

import Polygon exposing (setRotation)
import Shapes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (..)


{-| Regular Tiling of the plane with the `square` shape.

  - Type: regular
  - Corners: **4.4.4.4**
  - Symmetry: square

-}
squareTiling : Int -> Int -> Point -> List (Svg msg)
squareTiling n m origin =
    let
        size =
            30.0
    in
    List.range 0 m
        |> List.map
            (\y ->
                List.range 0 n
                    |> List.map (\x -> ( add origin { x = size * toFloat x, y = size * toFloat y }, mix3Color (y + x) ))
                    |> List.concatMap (\( point, color ) -> renderShape squareShape size point [ color ])
            )
        |> List.concat


squareShape : Shape
squareShape =
    asShape
        [ square ]


{-| Regular Tiling of the plane with the `triangle` shape.

  - Type: regular
  - Corners: **3.3.3.3.3.3**
  - Symmetry: hexagonal

-}
triangularTiling : Int -> Int -> Point -> List (Svg msg)
triangularTiling n m origin =
    let
        size =
            40.0
    in
    List.range 0 m
        |> List.map
            (\y ->
                List.range 0 n
                    |> List.map
                        (\x ->
                            ( add origin
                                { x = size * toFloat x + size / 2 * toFloat (modBy 2 (y + 1)) + size / 2 * toFloat (modBy 2 ((y + 1) // 2))
                                , y = (sqrt 3 * size / 2) * toFloat (y // 2)
                                }
                            , mix4Color y
                            )
                        )
                    |> List.concatMap
                        (\( point, color ) ->
                            case modBy 2 y of
                                0 ->
                                    renderShape (asShape [ equilateral ]) size point [ color ]

                                _ ->
                                    renderShape (asShape [ setRotation 60 equilateral ]) size point [ color ]
                        )
            )
        |> List.concat


{-| Regular Tiling of the plane with the `hexagon` shape.

  - Type: regular
  - Corners: **6.6.6**
  - Symmetry: hexagonal

-}
hexagonalTiling : Int -> Int -> Point -> List (Svg msg)
hexagonalTiling n m origin =
    let
        size =
            30.0
    in
    List.range 0 m
        |> List.map
            (\y ->
                List.range 0 n
                    |> List.map
                        (\x ->
                            ( add origin
                                { x = size * sqrt 3 * toFloat x + size / 2 * sqrt 3 * toFloat (modBy 2 (y + 1))
                                , y = size * 3 / 2 * toFloat y
                                }
                            , mix2Color y
                            )
                        )
                    |> List.concatMap
                        (\( point, color ) -> renderShape (asShape [ setRotation 30 hexagon ]) size point [ color ])
            )
        |> List.concat
