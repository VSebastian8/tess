module RuleBased.Regular exposing (..)

import Polygon exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import Util exposing (..)


regularTesselations : List ( String, Tess )
regularTesselations =
    [ ( "Square", squareTessellation )
    , ( "Rotated Square", rotatedSquareTessellation )
    , ( "Square Flower", squareFlowerTessellation )
    , ( "Triangular", triangularTessellation )
    , ( "Rotated Triangular", rotatedTriangularTessellation )
    , ( "Hexagonal", hexagonalTessellation )
    ]


squareTessellation : Tess
squareTessellation =
    let
        squareRule1 =
            { anchor = squ
            , additions =
                [ tr { x = 1, y = 0 } { squ | col = Secondary }
                , tr { x = 0, y = 1 } { squ | col = Secondary }
                ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 3, y = 3 } )
            }

        squareRule2 =
            { anchor = { squ | col = Secondary }
            , additions =
                [ tr { x = 1, y = 0 } { squ | col = Ternary }
                , tr { x = 0, y = 1 } { squ | col = Ternary }
                ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 3, y = 3 } )
            }

        squareRule3 =
            { anchor = { squ | col = Ternary }
            , additions = [ tr { x = 1, y = 0 } squ, tr { x = 0, y = 1 } squ ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 3, y = 3 } )
            }
    in
    { rules =
        [ squareRule1
        , squareRule2
        , squareRule3
        ]
    , open = [ squ ]
    , closed = []
    , size = 30
    }


triangularTessellation : Tess
triangularTessellation =
    let
        triRule1 =
            { anchor = eqi
            , additions =
                [ { eqi | col = Quart } |> rto 60 |> tr (equilateral |> getPoint 2)
                , tr { x = 1, y = 0 } eqi
                ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 2, y = 1 } )
            }

        triRule2 =
            { anchor = eqi
            , additions =
                [ { eqi | col = Secondary } |> rto -60 |> tr (equilateral |> getPoint 2) ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 1, y = 2 } )
            }

        triRule3 =
            { anchor = { eqi | col = Secondary } |> rto -60
            , additions = [ { eqi | col = Ternary } ]
            , rotatable = False
            , bounds = ( { x = -1, y = 0 }, { x = 2, y = 1 } )
            }

        triRule4 =
            { anchor = { eqi | col = Secondary } |> rto -60
            , additions = [ eqi |> tr (equilateral |> setRotation -60 |> getPoint 2) ]
            , rotatable = False
            , bounds = ( { x = -0.5, y = 0 }, { x = 1, y = 2 } )
            }
    in
    { rules = [ triRule1, triRule2, triRule3, triRule4 ]
    , open = [ eqi |> tr { x = -0.5, y = 0 } ]
    , closed = []
    , size = 30
    }


hexagonalTessellation : Tess
hexagonalTessellation =
    let
        hexRule1 =
            { anchor = hex
            , additions = [ { hex | col = Secondary } |> tr (hex.poly |> getPoint 2) ]
            , rotatable = False
            , bounds = ( { x = 0, y = -0.2 }, { x = 3.2, y = 3 } )
            }

        hexRule2 =
            { anchor = { hex | col = Secondary }
            , additions = [ { hex | col = Ternary } |> tr (hex.poly |> getPoint 2) ]
            , rotatable = False
            , bounds = ( { x = 0, y = -0.2 }, { x = 3.2, y = 3 } )
            }

        hexRule3 =
            { anchor = { hex | col = Ternary }
            , additions = [ hex |> tr (hex.poly |> getPoint 2) ]
            , rotatable = False
            , bounds = ( { x = 0, y = -0.2 }, { x = 3.2, y = 3 } )
            }

        hexRule4 =
            { anchor = hex
            , additions = [ { hex | col = Secondary } |> tr (hex.poly |> getPoint 4) |> tr (hex.poly |> getPoint 2 |> neg) ]
            , rotatable = False
            , bounds = ( { x = -1, y = -0.2 }, { x = 3.2, y = 3.2 } )
            }

        hexRule5 =
            { anchor = { hex | col = Ternary }
            , additions =
                [ hex |> tr (hex.poly |> getPoint 4) |> tr (hex.poly |> getPoint 2 |> neg) ]
            , rotatable = False
            , bounds = ( { x = -1, y = -0.2 }, { x = 3.2, y = 3.2 } )
            }
    in
    { rules = [ hexRule1, hexRule2, hexRule3, hexRule4, hexRule5 ]
    , open =
        [ hex |> tr { x = -0.5, y = -0.5 }
        ]
    , closed = []
    , size = 30
    }


rotatedSquareTessellation : Tess
rotatedSquareTessellation =
    let
        squareRule1 =
            { anchor = squ
            , additions =
                [ tr { x = 1, y = 0 } { squ | col = Secondary }
                , tr { x = 0, y = 1 } { squ | col = Secondary }
                ]
            , rotatable = True
            , bounds = ( { x = 0, y = 0 }, { x = 2.8, y = 2 } )
            }

        squareRule2 =
            { anchor = { squ | col = Secondary }
            , additions =
                [ tr { x = 1, y = 0 } { squ | col = Ternary }
                , tr { x = 0, y = 1 } { squ | col = Ternary }
                ]
            , rotatable = True
            , bounds = ( { x = 0, y = 0 }, { x = 2.8, y = 2 } )
            }

        squareRule3 =
            { anchor = { squ | col = Ternary }
            , additions = [ tr { x = 1, y = 0 } squ, tr { x = 0, y = 1 } squ ]
            , rotatable = True
            , bounds = ( { x = 0, y = 0 }, { x = 2.8, y = 2 } )
            }

        squareRule4 =
            { anchor = squ |> rto 45
            , additions = [ squ |> rto 45 |> tr (squ.poly |> setRotation 45 |> getPoint 3) |> tr (squ.poly |> setRotation 45 |> getPoint 1 |> neg) ]
            , rotatable = False
            , bounds = ( { x = 0.5, y = -1 }, { x = 1, y = 2.8 } )
            }
    in
    { rules =
        [ squareRule1
        , squareRule2
        , squareRule3
        , squareRule4
        ]
    , open = [ squ |> rto 45 |> tr { x = -1, y = 0 } ]
    , closed = []
    , size = 30
    }


rotatedTriangularTessellation : Tess
rotatedTriangularTessellation =
    let
        triRule1 =
            { anchor = eqi
            , additions =
                [ { eqi | col = Quart } |> rto 60 |> tr (equilateral |> getPoint 2)
                , tr { x = 1, y = 0 } eqi
                ]
            , rotatable = True
            , bounds = ( { x = 0, y = 0 }, { x = 2, y = 1 } )
            }

        triRule2 =
            { anchor = eqi |> tr { x = 1, y = 0 }
            , additions =
                [ { eqi | col = Secondary } |> rto -60 |> tr (equilateral |> getPoint 2) |> tr { x = -1, y = 0 } ]
            , rotatable = True
            , bounds = ( { x = -1, y = 0 }, { x = 3, y = 2 } )
            }

        triRule3 =
            { anchor = { eqi | col = Secondary } |> rto -60 |> tr { x = 0.5, y = 0 }
            , additions = [ { eqi | col = Ternary } |> tr { x = 0.5, y = 0 } ]
            , rotatable = True
            , bounds = ( { x = -0.5, y = 0 }, { x = 2, y = 1 } )
            }

        triRule4 =
            { anchor = { eqi | col = Secondary } |> rto -60 |> tr { x = 0.5, y = 0 }
            , additions = [ eqi |> tr (equilateral |> setRotation -60 |> getPoint 2) |> tr { x = -0.5, y = 0 } ]
            , rotatable = True
            , bounds = ( { x = -0.5, y = 0 }, { x = 1, y = 2 } )
            }
    in
    { rules = [ triRule1, triRule2, triRule3, triRule4 ]
    , open = [ eqi |> rto 45 |> tr { x = -1, y = 0 } ]
    , closed = []
    , size = 30
    }


squareFlowerTessellation : Tess
squareFlowerTessellation =
    let
        diag1 =
            { anchor = squ
            , additions =
                [ { squ | col = Secondary } |> tr { x = 1, y = 1 }
                ]
            , rotatable = True
            , bounds = ( { x = -1, y = -1 }, { x = 3, y = 3 } )
            }

        diag2 =
            { anchor = { squ | col = Secondary }
            , additions =
                [ { squ | col = Ternary } |> tr { x = 1, y = 1 }
                ]
            , rotatable = True
            , bounds = ( { x = -1, y = -1 }, { x = 3, y = 3 } )
            }

        diag3 =
            { anchor = { squ | col = Ternary }
            , additions =
                [ squ |> tr { x = 1, y = 1 }
                ]
            , rotatable = True
            , bounds = ( { x = -1, y = -1 }, { x = 3, y = 3 } )
            }

        fill1 =
            { anchor = squ
            , additions =
                [ squ |> tr { x = 0, y = -1 }
                ]
            , rotatable = True
            , bounds = ( { x = -1, y = -2 }, { x = 3, y = 3 } )
            }

        fill2 =
            { anchor = { squ | col = Secondary }
            , additions =
                [ { squ | col = Secondary } |> tr { x = 0, y = -1 }
                ]
            , rotatable = True
            , bounds = ( { x = -1, y = -2 }, { x = 3, y = 3 } )
            }

        fill3 =
            { anchor = { squ | col = Ternary }
            , additions =
                [ { squ | col = Ternary } |> tr { x = 0, y = -1 }
                ]
            , rotatable = True
            , bounds = ( { x = -1, y = -2 }, { x = 3, y = 3 } )
            }
    in
    { rules =
        [ diag1
        , diag2
        , diag3
        , fill1
        , fill2
        , fill3
        ]
    , open =
        [ { squ | col = Secondary } |> rto 180
        , { squ | col = Secondary } |> rto 90 |> tr { x = 1, y = 0 }
        , { squ | col = Secondary } |> tr { x = 1, y = 1 }
        , { squ | col = Secondary } |> rto -90 |> tr { x = 0, y = 1 }
        , squ
        ]
            |> List.map (tr { x = 12.8, y = 12.8 })
    , closed = []
    , size = 30
    }
