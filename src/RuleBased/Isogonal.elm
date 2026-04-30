module RuleBased.Isogonal exposing (..)

import Polygon exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import Util exposing (..)


isogonalTesselations : List ( String, Tess )
isogonalTesselations =
    [ ( "Square Rows", squareRowsTessellation )
    , ( "Triangular Rows", triangularRowsTessellation )
    , ( "Pythagorean", pythagoreanTessellation )
    , ( "Trithagorean", trithagoreanTessellation )
    , ( "Hexa Star", hexaStarTessellation )
    , ( "Hexa Gyra", hexaGyraTessellation )
    ]


squareRowsTessellation : Tess
squareRowsTessellation =
    let
        squareRule1 =
            { anchor = squ
            , additions =
                [ tr { x = 1, y = 0 } { squ | col = Quart }
                , tr { x = 2, y = 0 } squ
                ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 3, y = 1 } )
            }

        squareRule2 =
            { anchor = { squ | col = Ternary }
            , additions =
                [ tr { x = 1, y = 0 } { squ | col = Secondary }
                , tr { x = 2, y = 0 } { squ | col = Ternary }
                ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 3, y = 1 } )
            }

        squareRule3 =
            { anchor = squ
            , additions = [ tr { x = -0.5, y = 1 } { squ | col = Ternary } ]
            , rotatable = False
            , bounds = ( { x = -0.5, y = 0 }, { x = 2, y = 3 } )
            }

        squareRule4 =
            { anchor = { squ | col = Ternary }
            , additions = [ tr { x = 0.5, y = 1 } squ ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 2, y = 3 } )
            }
    in
    { rules =
        [ squareRule1
        , squareRule2
        , squareRule3
        , squareRule4
        ]
    , open = [ squ ]
    , closed = []
    , size = 30
    }


triangularRowsTessellation : Tess
triangularRowsTessellation =
    let
        triRule1 =
            { anchor = eqi
            , additions =
                [ { eqi | col = Secondary } |> rto 60 |> tr (equilateral |> getPoint 2)
                , tr { x = 1, y = 0 } eqi
                ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 2, y = 1 } )
            }

        triRule2 =
            { anchor = eqi
            , additions =
                [ eqi |> tr (equilateral |> getPoint 2) |> tr { x = -0.5, y = 0 } ]
            , rotatable = False
            , bounds = ( { x = 0, y = 0 }, { x = 1, y = 2 } )
            }
    in
    { rules = [ triRule1, triRule2 ]
    , open = [ eqi |> tr { x = -0.5, y = 0 } ]
    , closed = []
    , size = 30
    }


pythagoreanTessellation : Tess
pythagoreanTessellation =
    let
        squareRule1 =
            { anchor = squ |> sz 3
            , additions =
                [ { squ | col = Ternary } |> tr { x = 3, y = 0 }
                , { squ | col = Ternary } |> tr { x = 2, y = 3 } |> rt { x = 2.5, y = 3.5 } 90
                , { squ | col = Ternary } |> tr { x = -1, y = 2 }
                , { squ | col = Ternary } |> tr { x = 0, y = -1 } |> rt { x = 0.5, y = -0.5 } 90
                ]
            , rotatable = False
            , bounds = ( { x = -1, y = -1 }, { x = 5, y = 5 } )
            }

        squareRule2 =
            { anchor = { squ | col = Ternary }
            , additions =
                [ squ |> sz 3 |> tr { x = 1, y = -2 }
                , { squ | col = Secondary } |> sz 3 |> tr { x = -2, y = -3 }
                , squ |> sz 3 |> tr { x = -3, y = 0 }
                , { squ | col = Secondary } |> sz 3 |> tr { x = 0, y = 1 }
                ]
            , rotatable = False
            , bounds = ( { x = -3, y = -3 }, { x = 7, y = 7 } )
            }

        squareRule3 =
            { anchor = { squ | col = Ternary } |> rt { x = 0.5, y = 0.5 } 90
            , additions =
                [ { squ | col = Secondary } |> sz 3 |> tr { x = 1, y = -2 }
                , squ |> sz 3 |> tr { x = -2, y = -3 }
                , { squ | col = Secondary } |> sz 3 |> tr { x = -3, y = 0 }
                , squ |> sz 3 |> tr { x = 0, y = 1 }
                ]
            , rotatable = False
            , bounds = ( { x = -3, y = -3 }, { x = 7, y = 7 } )
            }
    in
    { rules =
        [ squareRule2
        , squareRule1
        , squareRule3
        ]
    , open = [ squ |> sz 3 |> tr { x = 39.5, y = 39.5 } ]
    , closed = []
    , size = 10
    }


trithagoreanTessellation : Tess
trithagoreanTessellation =
    let
        triRule1 =
            { anchor = eqi |> sz 3
            , additions =
                [ { eqi | col = Ternary } |> rto 60 |> tr { x = 2, y = 0 }
                , { eqi | col = Ternary }
                    |> rto 60
                    |> tr (equilateral |> getPoint 2 |> mul 3)
                , { eqi | col = Ternary }
                    |> rto 60
                    |> tr (equilateral |> setRotation 60 |> getPoint 1 |> neg)
                ]
            , rotatable = False
            , bounds = ( { x = -0.3, y = -1.2 }, { x = 4, y = 4 } )
            }

        triRule2 =
            { anchor = { eqi | col = Ternary } |> rto 60
            , additions =
                [ { eqi | col = Secondary } |> sz 2 |> rto 60 |> tr { x = -2, y = 0 }
                , { eqi | col = Secondary } |> sz 2 |> rto 60 |> tr (equilateral |> setRotation 60 |> getPoint 1)
                , { eqi | col = Secondary } |> sz 2 |> rto 60 |> tr (equilateral |> setRotation 60 |> getPoint 2) |> tr (equilateral |> setRotation 60 |> getPoint 1 |> mul 2 |> neg)
                ]
            , rotatable = False
            , bounds = ( { x = -1.7, y = -2.3 }, { x = 4, y = 4 } )
            }

        triRule3 =
            { anchor = { eqi | col = Secondary } |> sz 2 |> rto 60
            , additions =
                [ eqi |> sz 3
                , eqi |> sz 3 |> tr (equilateral |> setRotation 60 |> getPoint 1 |> mul 2) |> tr { x = -3, y = 0 }
                , eqi |> sz 3 |> tr (equilateral |> setRotation 60 |> getPoint 1 |> mul 2) |> tr (equilateral |> getPoint 2 |> neg)
                ]
            , rotatable = False
            , bounds = ( { x = -1.7, y = -2.5 }, { x = 5, y = 5 } )
            }
    in
    { rules =
        [ triRule1
        , triRule2
        , triRule3
        ]
    , open = [ eqi |> sz 3 |> tr { x = 25, y = 25 } ]
    , closed = []
    , size = 15
    }


hexaStarTessellation : Tess
hexaStarTessellation =
    let
        hx =
            hexv |> sz 2

        hexRule1 =
            { anchor = hx
            , additions =
                [ { eqi | col = Ternary } |> rto 60
                , { eqi | col = Ternary } |> tr (hx |> pt 1)
                , { eqi | col = Ternary } |> rto -60 |> tr (hx |> pt 2)
                , { eqi | col = Ternary } |> rto -120 |> tr (hx |> pt 3)
                , { eqi | col = Ternary } |> rto -180 |> tr (hx |> pt 4)
                , { eqi | col = Ternary } |> rto 120 |> tr (hx |> pt 5)
                ]
            , rotatable = True
            , bounds = ( { x = -1.2, y = -0.5 }, { x = 5, y = 5 } )
            }

        eq1 =
            { eqi | col = Ternary } |> rto 180

        triRule1 =
            { anchor = eq1
            , additions =
                [ { hx | col = Secondary } |> tr (eq1 |> pt 1)
                ]
            , rotatable = True
            , bounds = ( { x = -2.2, y = -0.5 }, { x = 5, y = 5 } )
            }

        hexRule2 =
            { anchor = { hx | col = Secondary }
            , additions =
                [ hx |> tr { x = -1, y = 0 } |> tr (pt 4 hx)
                , { eqi | col = Quart } |> rto -60 |> tr (hx |> pt 2)
                ]
            , rotatable = True
            , bounds = ( { x = -1.2, y = 0 }, { x = 5, y = 7 } )
            }
    in
    { rules =
        [ hexRule1
        , triRule1
        , hexRule2
        ]
    , open = [ hexv |> rto 0 |> sz 2 |> tr { x = 26, y = 26 } ]
    , closed = []
    , size = 15
    }


hexaGyraTessellation : Tess
hexaGyraTessellation =
    let
        hexRule =
            { anchor = hexv
            , additions =
                [ { eqi | col = Secondary } |> rto 60 |> sz 2
                , { eqi | col = Ternary } |> tr (pt 1 hexv) |> sz 2
                , { eqi | col = Secondary } |> rto -60 |> tr (pt 2 hexv) |> sz 2
                , { eqi | col = Ternary } |> rto -120 |> tr (pt 3 hexv) |> sz 2
                , { eqi | col = Secondary } |> rto -180 |> tr (pt 4 hexv) |> sz 2
                , { eqi | col = Ternary } |> rto -240 |> tr (pt 5 hexv) |> sz 2
                ]
            , rotatable = True
            , bounds = ( { x = -2, y = -1.5 }, { x = 5, y = 5 } )
            }

        triRule1 =
            { anchor = { eqi | col = Secondary } |> rto 60 |> sz 2
            , additions =
                [ hexv |> tr (eqi |> rto 60 |> sz 2 |> pt 1) |> tr { x = 1, y = 0 } ]
            , rotatable = True
            , bounds = ( { x = -1, y = -3 }, { x = 5, y = 5 } )
            }

        triRule2 =
            { anchor = { eqi | col = Ternary } |> sz 2
            , additions =
                [ hexv |> tr (eqi |> sz 2 |> pt 1) |> tr (hexv |> pt 5) ]
            , rotatable = True
            , bounds = ( { x = -1, y = 0 }, { x = 5, y = 5 } )
            }
    in
    { rules =
        [ hexRule
        , triRule1
        , triRule2
        ]
    , open = [ hexv |> rto 0 |> tr { x = 20, y = 20 } ]
    , closed = []
    , size = 20
    }
