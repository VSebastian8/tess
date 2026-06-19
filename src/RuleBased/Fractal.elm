module RuleBased.Fractal exposing (..)

import Polygon exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import Util exposing (..)


fractalTesselations : List ( String, Tess )
fractalTesselations =
    [ ( "Floret Fan", floretFractalTess )
    , ( "Square Diagonal", squareFractalTess )
    , ( "Sierpinski Triangle", sierpinskiTriangleTess )
    , ( "Kuvina Star", kuvinaStarTess )
    , ( "Hexagonal Recursion", hexagonalRecursionTess )
    , ( "Turtle Shell", turtleShellTess )
    ]


floretFractalTess : Tess
floretFractalTess =
    let
        petal1 =
            { r
                | anchor = flr
                , additions = [ flr |> rto -60 ]
                , bounds = ( { x = -0.75, y = 0 }, { x = 3, y = 2.5 } )
            }

        next1 =
            { r
                | anchor = flr
                , additions =
                    [ flr |> rto -120 |> tr (flr |> pt 2) |> tr (flr |> pt 1)
                    , flr |> rto 120 |> tr (flr |> pt 4) |> tr (flr |> rto 120 |> pt 2 |> neg)
                    ]
                , rotatable = False
                , bounds = ( { x = 0, y = 0 }, { x = 4.5, y = 4.5 } )
            }

        next2 =
            { r
                | anchor = flr |> rto -120 |> tr (flr |> pt 2) |> tr (flr |> pt 1)
                , additions =
                    [ flr |> rto 120 |> tr (flr |> pt 4) |> tr (flr |> rto 120 |> pt 2 |> neg)
                    , flr
                    ]
                , rotatable = False
                , bounds = ( { x = 0, y = 0 }, { x = 4.5, y = 4.5 } )
            }

        next3 =
            { r
                | anchor = flr |> rto 120 |> tr (flr |> pt 4) |> tr (flr |> rto 120 |> pt 2 |> neg)
                , additions =
                    [ flr
                    , flr |> rto -120 |> tr (flr |> pt 2) |> tr (flr |> pt 1)
                    ]
                , rotatable = False
                , bounds = ( { x = 0, y = 0 }, { x = 4.5, y = 4.5 } )
            }

        fractal =
            { r
                | anchor = flr |> rto -60
                , additions =
                    [ ois |> rto -120 |> tr (flr |> rto -60 |> pt 0.5)
                    , { rgt | col = Secondary } |> sc (1 + 2 * cos (degrees 60)) |> rto 60 |> tr (flr |> rto -60 |> pt 4)
                    , { eqi | col = Ternary } |> rto -180 |> tr (flr |> rto -60 |> pt 1)
                    , { eqi | col = Quart } |> tr (flr |> rto -60 |> pt 4)
                    , { rho | col = Quart } |> tr (flr |> rto -60 |> pt 1) |> tr { x = -1, y = 0 }
                    ]
                , rotatable = True
                , subdivide = True
                , bounds = ( { x = -1, y = 0 }, { x = 2, y = 2.5 } )
            }
    in
    { t
        | rules = [ petal1, next1, next2, next3, fractal ]
        , open =
            [ flr
            , flr |> rto -120 |> tr (flr |> pt 2) |> tr (flr |> pt 1)
            , flr |> rto 120 |> tr (flr |> pt 4) |> tr (flr |> rto 120 |> pt 2 |> neg)
            ]
        , size = 25
        , margin = ( 3, 3 )
    }


squareFractalTess : Tess
squareFractalTess =
    let
        grow =
            { r
                | additions = [ squ |> sc 2 |> tr { x = -0.5, y = -0.5 } ]
                , subdivide = True
                , bounds = ( { x = -0.5, y = -0.5 }, { x = 2, y = 2 } )
            }

        shrink =
            { r
                | anchor = squ
                , additions = [ { squ | col = Secondary } ]
                , subdivide = True
                , bounds = ( { x = 0, y = 0 }, { x = 1, y = 1 } )
            }

        diag =
            { r
                | anchor = { squ | col = Secondary }
                , additions =
                    [ { squ | col = Secondary } |> sc (1 / 2)
                    , { squ | col = Secondary } |> sc (1 / 2) |> tr { x = 0.5, y = 0.5 }
                    , { squ | col = Ternary } |> sc (1 / 2) |> tr { x = 0.5, y = 0 }
                    , { squ | col = Ternary } |> sc (1 / 2) |> tr { x = 0, y = 0.5 }
                    ]
                , fragment = 1
                , subdivide = True
                , bounds = ( { x = 0, y = 0 }, { x = 1, y = 1 } )
            }
    in
    { t | rules = [ grow, shrink, diag ], open = [ squ |> sc 1.2 |> tr { x = -0.1, y = -0.1 } ], size = 10 }


sierpinskiTriangleTess : Tess
sierpinskiTriangleTess =
    let
        grow =
            { r
                | anchor = eqi |> rto 60
                , additions = [ eqi |> sc 2 |> rto 60 |> tr { x = -0.5, y = sqrt 3 / 4 } ]
                , subdivide = True
                , bounds = ( { x = -0.5, y = -1.4 }, { x = 2, y = 2 } )
            }

        shrink =
            { r
                | anchor = eqi
                , additions = [ { eqi | col = Secondary } ]
                , subdivide = True
                , bounds = ( { x = 0, y = 0 }, { x = 1, y = 1 } )
            }

        recursion =
            { r
                | anchor = { eqi | col = Secondary } |> rto 60 |> sc 2
                , additions =
                    [ { eqi | col = Secondary } |> rto 60 |> tr (eqi |> rto 60 |> pt 1)
                    , { eqi | col = Secondary } |> rto 60
                    , { eqi | col = Secondary } |> rto 60 |> tr { x = 1, y = 0 }
                    , { eqi | col = Ternary } |> tr (eqi |> rto 60 |> pt 1)
                    ]
                , subdivide = True
                , fragment = 2
                , bounds = ( { x = 0, y = -1.7 }, { x = 2, y = 2 } )
            }
    in
    { t | rules = [ grow, shrink, recursion ], open = [ eqi |> sc 1.2 |> rto 60 ], size = 10 }


hexagonalRecursionTess : Tess
hexagonalRecursionTess =
    let
        grow =
            { r
                | anchor = hex
                , additions = [ hex |> sc 2 |> tr (hex.centre |> neg) ]
                , subdivide = True
                , bounds = ( { x = -0.86, y = -0.5 }, { x = 3.4, y = 2 } )
            }

        small_hex =
            { hex | col = Secondary } |> sc 0.8

        shrink =
            { r
                | anchor = hex
                , additions = [ small_hex |> tr hex.centre |> tr (small_hex.centre |> neg) ]
                , subdivide = True
                , bounds = ( { x = 0, y = 0 }, { x = 1.7, y = 1 } )
            }

        recursion =
            { r
                | anchor = { hex | col = Secondary } |> sc 2
                , additions =
                    [ { rho | col = Ternary } |> rto -30 |> tr (hex |> pt 1 |> mul 2)
                    , { rho | col = Ternary } |> rto -150 |> tr (hex |> pt 3 |> mul 2)
                    , { rho | col = Ternary } |> rto 90 |> tr (hex |> pt 5 |> mul 2)
                    , { hex | col = Secondary }
                    , { hex | col = Secondary } |> tr (hex |> pt 2)
                    , { hex | col = Secondary } |> tr (hex |> pt 4)
                    ]
                , subdivide = True
                , fragment = 0.8
                , bounds = ( { x = 0, y = 0 }, { x = 3.4, y = 2 } )
            }
    in
    { t | rules = [ grow, shrink, recursion ], open = [ hex ], size = 15 }


turtleShellTess : Tess
turtleShellTess =
    let
        hexTile =
            { r
                | anchor = { hex | col = Ternary }
                , additions =
                    [ { hex | col = Ternary } |> tr (hex |> pt 4 |> neg)
                    , { hex | col = Ternary } |> tr (hex |> pt 1) |> tr { x = 0, y = -1 }
                    , { hex | col = Ternary } |> tr (hex |> pt 2)
                    , { hex | col = Ternary } |> tr (hex |> pt 4)
                    , { hex | col = Ternary } |> tr (hex |> pt 1 |> neg) |> tr { x = 0, y = 1 }
                    , { hex | col = Ternary } |> tr (hex |> pt 2 |> neg)
                    ]
                , bounds = ( { x = -1.6, y = -2 }, { x = 5, y = 5 } )
            }

        turtle =
            { r
                | anchor = { hex | col = Ternary } |> sc 2
                , additions =
                    [ hex
                    , { hex | col = Secondary } |> tr (hex |> pt 2)
                    , { hex | col = Ternary } |> tr (hex |> pt 4)
                    , { rho | col = Ternary } |> rto -30 |> tr (hex |> pt 1 |> mul 2)
                    , { rho | col = Primary } |> rto -150 |> tr (hex |> pt 3 |> mul 2)
                    , { rho | col = Secondary } |> rto 90 |> tr (hex |> pt 5 |> mul 2)
                    ]
                , subdivide = True
                , bounds = ( { x = 0, y = 0 }, { x = 3.4, y = 2 } )
            }
    in
    { t | rules = [ hexTile, turtle ], open = [ { hex | col = Ternary } ], size = 40, margin = ( 1, 1 ) }


kuvinaStarTess : Tess
kuvinaStarTess =
    let
        triTile1 =
            { r
                | anchor = eqi
                , additions =
                    [ eqi |> rto -60 |> tr (eqi |> pt 1)
                    , eqi |> rto -60
                    , eqi |> tr (eqi |> pt 1)
                    , eqi |> tr (eqi |> pt 1 |> neg)
                    ]
                , rotatable = False
                , bounds = ( { x = -1, y = 0 }, { x = 3, y = 1 } )
            }

        triTile2 =
            { r
                | anchor = eqi
                , additions =
                    [ eqi |> tr (eqi |> pt 2)
                    , eqi |> tr (eqi |> pt 2 |> neg)
                    ]
                , rotatable = False
                , bounds = ( { x = -1, y = 0 }, { x = 3, y = 1 } )
            }

        bigT =
            eqi |> sc (2 * cos (degrees 30))

        subRule =
            { r
                | anchor = bigT
                , additions =
                    [ ois |> rto 150 |> tr bigT.centre
                    , ois |> rto 30 |> tr bigT.centre
                    , ois |> rto -90 |> tr bigT.centre
                    ]
                , bounds = ( { x = 0, y = 0 }, { x = 1.7, y = 1 } )
                , subdivide = True
            }

        bigO =
            ois |> rto -30 |> sc 2.7

        hO =
            sin (degrees 30) * 2.7

        hP =
            sin (degrees 30) / (2 * cos (degrees 30))

        rL =
            1 / (2 * cos (degrees 30))

        sca =
            hO / (hP + rL + 1)

        tS =
            2.7 - (2 * cos (degrees 30)) - rL * sca

        tS2 =
            cos (degrees 30) * 2.7 - 1 - tS - sca / 2

        penta1 =
            { lengths = [ tS, tS + 0.02, tS + 0.04, tS2, 1 - tS2 ]
            , angles = [ 120, 90, 120, 120, 90 ]
            , rotation = 0
            , origin = { x = 0, y = 0 }
            }

        penta2 =
            { lengths = [ 1 - tS2, tS2 + 0.02, tS, tS + 0.04, tS2 ]
            , angles = [ 120, 120, 90, 120, 90 ]
            , rotation = 0
            , origin = { x = 0, y = 0 }
            }

        pen1 =
            { poly = penta1, col = Primary, centre = { x = tS / 2, y = tS / 2 }, dist = tS2 / 2, scale = 1 }

        pen2 =
            { poly = penta2, col = Primary, centre = { x = tS / 2, y = tS / 2 }, dist = tS2 / 2, scale = 1 }

        kuvi =
            { r
                | anchor = bigO
                , additions =
                    [ rho |> rto -90 |> sc rL |> sc sca |> tr (rho |> rto -90 |> sc rL |> sc sca |> pt 3 |> neg)
                    , rho |> rto -150 |> sc rL |> sc sca |> tr { x = 0, y = sca * rL }
                    , { pri | col = Ternary } |> rto 180 |> sc sca |> tr (bigO |> pt 1.5) |> tr { x = sca / 2, y = 0 }
                    , { eqi | col = Secondary } |> rto -90 |> sc tS |> tr (rho |> rto -150 |> sc rL |> sc sca |> pt 1)
                    , { eqi | col = Secondary } |> rto -30 |> sc tS |> tr (rho |> rto -30 |> sc rL |> sc sca |> pt 1)
                    , { pen1 | col = Quart } |> rto -30 |> tr (bigO |> pt 2) |> tr (ois |> rto 180 |> pt 2) |> tr { x = 1, y = 0 }
                    , { pen2 | col = Quart } |> rto -60 |> tr (bigO |> pt 1) |> tr (ois |> rto 120 |> pt 1) |> tr { x = -1, y = 0 }
                    , { eqi | col = Secondary } |> rto 60 |> sc tS2 |> tr (bigO |> pt 2) |> tr { x = 1, y = 0 }
                    , { eqi | col = Secondary } |> rto 180 |> sc tS2 |> tr (bigO |> pt 1) |> tr { x = -1, y = 0 }
                    , ois |> rto 120 |> tr (bigO |> pt 1) |> tr { x = -1, y = 0 }
                    , ois |> rto 180 |> tr (bigO |> pt 2) |> tr { x = 1, y = 0 }
                    ]
                , subdivide = True
                , bounds = ( { x = -2.5, y = -0.5 }, { x = 5, y = 2 } )
                , fragment = 0.5
            }
    in
    { t
        | rules =
            [ triTile1
            , triTile2
            , subRule
            , kuvi
            ]
        , open = [ eqi ]
        , size = 170
        , margin = ( 1, 1 )
    }
