module RuleBased.Fractal exposing (..)

import Polygon exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import Util exposing (..)


fractalTesselations : List ( String, Tess )
fractalTesselations =
    [ ( "Floret Fan", floretFractalTessellation )
    ]


floretFractalTessellation : Tess
floretFractalTessellation =
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
