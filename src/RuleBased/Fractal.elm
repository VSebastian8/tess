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
    { rules = [ petal1, next1, next2, next3, fractal ]
    , open =
        [ flr
        , flr |> rto -120 |> tr (flr |> pt 2) |> tr (flr |> pt 1)
        , flr |> rto 120 |> tr (flr |> pt 4) |> tr (flr |> rto 120 |> pt 2 |> neg)
        ]
    , closed = []
    , start = { x = 0.5, y = 0.5 }
    , size = 25
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
                , fragment = 2
                , subdivide = True
                , bounds = ( { x = 0, y = 0 }, { x = 1, y = 1 } )
            }
    in
    { rules = [ grow, shrink, diag ], open = [ squ |> sc 1.2 |> tr { x = -0.1, y = -0.1 } ], closed = [], start = { x = 0.5, y = 0.5 }, size = 10 }


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
    { rules = [ grow, shrink, recursion ], open = [ eqi |> sc 1.2 |> rto 60 ], closed = [], start = { x = 0.5, y = 0.5 }, size = 10 }
