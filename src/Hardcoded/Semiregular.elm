module Hardcoded.Semiregular exposing (elongatedTriangular, rhombiTriHexagonalTiling, snubSquareTiling, snubTriHexagonalTiling, triHexagonalTiling, truncatedHexagonalTiling, truncatedSquareTiling, truncatedTriHexagonalTiling)

import Polygon exposing (..)
import Shapes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Util exposing (..)


{-| Truncating the corners of the hexagon (adding triangles at each corner)

  - Type: semiregular
  - Corners: **3.12.12**
  - Symmetry: hexagonal

-}
truncatedHexagonalTiling : Int -> Int -> Point -> List (Svg msg)
truncatedHexagonalTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                20

            decagon_width =
                sub (getPoint 3 dodecagon) (getPoint 0 dodecagon) |> mul size

            next_origin =
                if modBy 2 m == 0 then
                    add origin (getPoint 7 dodecagon |> mul size)

                else
                    sub (add origin (getPoint 8 dodecagon |> mul size)) decagon_width
        in
        truncatedHexagonalLine n origin ++ truncatedHexagonalTiling n (m - 1) next_origin


truncatedHexagonalLine : Int -> Point -> List (Svg msg)
truncatedHexagonalLine n origin =
    if n <= 0 then
        []

    else
        let
            size =
                20

            first_trig =
                getPoint 3 dodecagon

            next_origin =
                add origin
                    (add first_trig (getPoint 1 equilateral) |> mul size)
        in
        renderShape truncatedHexagonalShape size origin [ Primary, Secondary, Secondary ]
            ++ truncatedHexagonalLine (n - 1) next_origin


truncatedHexagonalShape : Shape
truncatedHexagonalShape =
    let
        first_trig =
            getPoint 3 dodecagon

        second_trig =
            getPoint 6 dodecagon
    in
    [ dodecagon
    , equilateral |> setOrigin first_trig
    , equilateral |> setRotation 60 |> setOrigin second_trig
    ]
        |> asShape


{-| Rectification of the hexagon (the original edges vanish)

  - Type: semiregular
  - Corners: **3.6.3.6**
  - Symmetry: hexagonal

-}
triHexagonalTiling : Int -> Int -> Point -> List (Svg msg)
triHexagonalTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            hexagon_translate =
                { x = size * 2, y = 0 }

            next_origin =
                if modBy 2 m == 0 then
                    add origin (getPoint 3 hexagon |> mul size)

                else
                    sub (add origin (getPoint 3 hexagon |> mul size)) hexagon_translate
        in
        triHexagonalLine n origin ++ triHexagonalTiling n (m - 1) next_origin


triHexagonalLine : Int -> Point -> List (Svg msg)
triHexagonalLine n origin =
    if n <= 0 then
        []

    else
        let
            size =
                30

            first_trig =
                getPoint 1 hexagon

            next_origin =
                add origin
                    (add first_trig (getPoint 1 equilateral) |> mul size)
        in
        renderShape triHexagonalShape size origin [ Primary, Secondary, Secondary ]
            ++ triHexagonalLine (n - 1) next_origin


triHexagonalShape : Shape
triHexagonalShape =
    let
        first_trig =
            getPoint 1 hexagon

        second_trig =
            getPoint 3 hexagon
    in
    [ hexagon
    , equilateral |> setOrigin first_trig
    , equilateral |> setRotation 60 |> setOrigin second_trig
    ]
        |> asShape


{-| Truncation of the square tiling

  - Type: semiregular
  - Corners: **4.8.8**
  - Symmetry: square

-}
truncatedSquareTiling : Int -> Int -> Point -> List (Svg msg)
truncatedSquareTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                20

            next_origin =
                add origin (getPoint 5 octagon |> mul size)
        in
        truncatedSquareLine n origin ++ truncatedSquareTiling n (m - 1) next_origin


truncatedSquareLine : Int -> Point -> List (Svg msg)
truncatedSquareLine n origin =
    if n <= 0 then
        []

    else
        let
            size =
                20

            top_square =
                getPoint 1 octagon

            next_origin =
                add origin
                    (add top_square
                        (square |> setRotation 45 |> getPoint 2)
                        |> mul size
                    )
        in
        renderShape truncatedSquareShape size origin [ Primary, Secondary ]
            ++ truncatedSquareLine (n - 1) next_origin


truncatedSquareShape : Shape
truncatedSquareShape =
    let
        top_square =
            getPoint 1 octagon
    in
    [ octagon
    , square |> setRotation 45 |> setOrigin top_square
    ]
        |> asShape


{-| Rectification of the trihexagonal tiling

     - Type: semiregular
     - Corners: **3.4.6.4**
     - Symmetry: hexagonal

-}
rhombiTriHexagonalTiling : Int -> Int -> Point -> List (Svg msg)
rhombiTriHexagonalTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            next_point =
                if modBy 2 m == 0 then
                    sub (add (getPoint -1 hexagon) (square |> setRotation -60 |> getPoint -1)) { x = 1, y = 0 }

                else
                    add (getPoint 2 hexagon) (square |> setRotation -30 |> getPoint 1)

            next_origin =
                next_point |> mul size |> add origin
        in
        rhombiTriHexaLine n origin size ++ rhombiTriHexagonalTiling n (m - 1) next_origin


rhombiTriHexaLine : Int -> Point -> Float -> List (Svg msg)
rhombiTriHexaLine n origin size =
    if n <= 0 then
        []

    else
        let
            hexagon_width =
                sub (getPoint 2 hexagon) (getPoint -1 hexagon) |> mul size

            square_slant =
                { x =
                    (sub (square |> setRotation -30 |> getPoint 1) (square |> setRotation -30 |> getPoint 0)).x
                , y = 0
                }
                    |> mul size

            next_origin =
                add origin (add hexagon_width (add square_slant (add { x = size, y = 0 } square_slant)))
        in
        renderShape rhombiTriHexaShape size origin [ Primary, Ternary, Secondary, Ternary, Secondary, Ternary ]
            ++ rhombiTriHexaLine (n - 1) next_origin size


rhombiTriHexaShape : Shape
rhombiTriHexaShape =
    let
        square1 =
            getPoint 2 hexagon

        trig1 =
            getPoint 3 hexagon

        square2 =
            getPoint 4 hexagon

        trig2 =
            getPoint 4 hexagon

        square3 =
            getPoint 5 hexagon
    in
    [ hexagon
    , square |> setRotation -30 |> setOrigin square1
    , equilateral |> setRotation -30 |> setOrigin trig1
    , square |> setOrigin square2
    , equilateral |> setRotation -90 |> setOrigin trig2
    , square |> setRotation -60 |> setOrigin square3
    ]
        |> asShape


{-| Truncation of the trihexagonal tiling

     - Type: semiregular
     - Corners: **4.6.12**
     - Symmetry: hexagonal

-}
truncatedTriHexagonalTiling : Int -> Int -> Point -> List (Svg msg)
truncatedTriHexagonalTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                20

            hexagon_width =
                sub (hexagon |> setRotation 30 |> getPoint 2) (hexagon |> setRotation 30 |> getPoint 0)

            next_point =
                if modBy 2 m == 0 then
                    sub (add (getPoint 10 dodecagon) (square |> setRotation -60 |> getPoint -1))
                        (add hexagon_width { x = 1, y = 0 })

                else
                    add (getPoint 6 dodecagon) (square |> setRotation 60 |> getPoint 2)

            next_origin =
                next_point |> mul size |> add origin
        in
        truncTriHexaLine n origin size ++ truncatedTriHexagonalTiling n (m - 1) next_origin


truncTriHexaLine : Int -> Point -> Float -> List (Svg msg)
truncTriHexaLine n origin size =
    if n <= 0 then
        []

    else
        let
            dodecagon_width =
                sub (getPoint 4 dodecagon) (getPoint -1 dodecagon)

            hexagon_width =
                sub (hexagon |> setRotation 30 |> getPoint 2) (hexagon |> setRotation 30 |> getPoint 0)

            next_origin =
                dodecagon_width |> add hexagon_width |> add hexagon_width |> add { x = 1, y = 0 } |> mul size |> add origin
        in
        renderShape truncTriHexaShape size origin [ Primary, Ternary, Ternary, Ternary, Secondary, Secondary ]
            ++ truncTriHexaLine (n - 1) next_origin size


truncTriHexaShape : Shape
truncTriHexaShape =
    let
        square1 =
            getPoint 6 dodecagon

        square2 =
            getPoint 8 dodecagon

        square3 =
            getPoint 10 dodecagon

        hex1 =
            getPoint 7 dodecagon

        hex2 =
            getPoint 9 dodecagon
    in
    [ dodecagon
    , square |> setRotation 60 |> setOrigin square1
    , square |> setOrigin square2
    , square |> setRotation -60 |> setOrigin square3
    , hexagon |> setRotation 30 |> setOrigin hex1
    , hexagon |> setRotation -30 |> setOrigin hex2
    ]
        |> asShape


{-| Half truncation of the truncated square tiling

     - Type: semiregular
     - Corners: **3.3.4.3.4**
     - Symmetry: crosshatch

-}
snubSquareTiling : Int -> Int -> Point -> List (Svg msg)
snubSquareTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            square_diag =
                sub (square |> setRotation 30 |> getPoint 2) (square |> setRotation 30 |> getPoint 0)

            next_origin =
                (if modBy 2 m == 0 then
                    add square_diag { x = -2 * square_diag.x, y = 1 }

                 else
                    add square_diag { x = 0, y = 1 }
                )
                    |> mul size
                    |> add origin
        in
        snubSquareLine n origin size ++ snubSquareTiling n (m - 1) next_origin


snubSquareLine : Int -> Point -> Float -> List (Svg msg)
snubSquareLine n origin size =
    if n <= 0 then
        []

    else
        let
            triangle_slant =
                sub (equilateral |> setRotation 90 |> getPoint 2) (equilateral |> setRotation 90 |> getPoint 0)

            next_origin =
                { x = origin.x + 2 * size * triangle_slant.x + size, y = origin.y }
        in
        renderShape snubSquareShape size origin [ Primary, Secondary, Primary, Primary, Primary, Secondary ]
            ++ snubSquareLine (n - 1) next_origin size


snubSquareShape : Shape
snubSquareShape =
    let
        first =
            equilateral |> setRotation 90 |> getPoint 2

        second =
            add first { x = 1, y = 0 }
    in
    [ equilateral |> setRotation 90
    , square |> setRotation 30
    , equilateral |> setOrigin first
    , equilateral |> setRotation 60 |> setOrigin first
    , equilateral |> setRotation 30 |> setOrigin second
    , square |> setRotation -30 |> setOrigin second
    ]
        |> asShape


{-| Half truncation of the trihexagonal tiling

  - Type: semiregular
  - Corners: **3.3.3.3.6**
  - Symmetry: hex twist

-}
snubTriHexagonalTiling : Int -> Int -> Point -> List (Svg msg)
snubTriHexagonalTiling n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            next_origin =
                (case modBy 3 m of
                    0 ->
                        sub (getPoint -1 hexagon) { x = 2, y = 0 }

                    1 ->
                        add (getPoint 2 hexagon) { x = 3, y = 0 }

                    _ ->
                        sub (getPoint -1 hexagon) { x = 2, y = 0 }
                )
                    |> mul size
                    |> add origin
        in
        snubTriHexagonalLine n origin size ++ snubTriHexagonalTiling n (m - 1) next_origin


snubTriHexagonalLine : Int -> Point -> Float -> List (Svg msg)
snubTriHexagonalLine n origin size =
    if n <= 0 then
        []

    else
        let
            next_origin =
                { x = origin.x + 7 * size, y = origin.y }
        in
        renderShape snubTriHexagonalShape
            size
            origin
            [ Primary, Secondary, Secondary, Secondary, Secondary, Secondary, Secondary, Secondary, Secondary ]
            ++ snubTriHexagonalLine (n - 1) next_origin size


snubTriHexagonalShape : Shape
snubTriHexagonalShape =
    let
        first =
            getPoint 3 hexagon

        second =
            getPoint 4 hexagon

        third =
            getPoint 5 hexagon
    in
    [ hexagon
    , equilateral |> setRotation 60 |> setOrigin first
    , equilateral |> setOrigin first
    , equilateral |> setRotation -60 |> setOrigin first
    , equilateral |> setOrigin second
    , equilateral |> setRotation -60 |> setOrigin second
    , equilateral |> setRotation -120 |> setOrigin second
    , equilateral |> setRotation -60 |> setOrigin third
    , equilateral |> setRotation -120 |> setOrigin third
    ]
        |> asShape


{-| Only non Wythoffian semiregular tiling

     - Type: semiregular
     - Corners: **3.3.3.4.4**
     - Symmetry: running bond

-}
elongatedTriangular : Int -> Int -> Point -> List (Svg msg)
elongatedTriangular n m origin =
    if m <= 0 then
        []

    else
        let
            size =
                30

            squareLine =
                List.range 0 n |> List.concatMap (\i -> renderShape (asShape [ square ]) size { x = origin.x + toFloat i * size, y = origin.y } [ Secondary ])

            triangleLine =
                List.range 0 n
                    |> List.concatMap
                        (\i ->
                            renderShape (asShape [ equilateral |> setRotation -60 ]) size { x = origin.x + toFloat i * size, y = origin.y } [ Primary ]
                                ++ renderShape (asShape [ equilateral ]) size { x = origin.x + toFloat i * size, y = origin.y } [ Ternary ]
                        )

            selectedLine =
                if modBy 2 m == 0 then
                    squareLine

                else
                    triangleLine

            next_origin =
                (case modBy 4 m of
                    0 ->
                        { x = 0, y = 1 }

                    1 ->
                        (equilateral |> setRotation -60) |> getPoint 1

                    2 ->
                        { x = 0, y = 1 }

                    _ ->
                        equilateral |> setRotation -60 |> getPoint -1
                )
                    |> mul size
                    |> add origin
        in
        selectedLine ++ elongatedTriangular n (m - 1) next_origin
