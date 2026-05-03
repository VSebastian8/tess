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
            { r
                | anchor = squ
                , additions =
                    [ tr { x = 1, y = 0 } { squ | col = Quart }
                    , tr { x = 2, y = 0 } squ
                    ]
                , rotatable = False
                , bounds = ( { x = 0, y = 0 }, { x = 3, y = 1 } )
            }

        squareRule2 =
            { r
                | anchor = { squ | col = Ternary }
                , additions =
                    [ tr { x = 1, y = 0 } { squ | col = Secondary }
                    , tr { x = 2, y = 0 } { squ | col = Ternary }
                    ]
                , rotatable = False
                , bounds = ( { x = 0, y = 0 }, { x = 3, y = 1 } )
            }

        squareRule3 =
            { r
                | anchor = squ
                , additions = [ tr { x = -0.5, y = 1 } { squ | col = Ternary } ]
                , rotatable = False
                , bounds = ( { x = -0.5, y = 0 }, { x = 2, y = 3 } )
            }

        squareRule4 =
            { r
                | anchor = { squ | col = Ternary }
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
    , start = { x = 0, y = 0 }
    }


triangularRowsTessellation : Tess
triangularRowsTessellation =
    let
        triRule1 =
            { r
                | anchor = eqi
                , additions =
                    [ { eqi | col = Secondary } |> rto 60 |> tr (equilateral |> getPoint 2)
                    , tr { x = 1, y = 0 } eqi
                    ]
                , rotatable = False
                , bounds = ( { x = 0, y = 0 }, { x = 2, y = 1 } )
            }

        triRule2 =
            { r
                | anchor = eqi
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
    , start = { x = 0, y = 0 }
    }


pythagoreanTessellation : Tess
pythagoreanTessellation =
    let
        squareRule1 =
            { r
                | anchor = squ |> sc 3
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
            { r
                | anchor = { squ | col = Ternary }
                , additions =
                    [ squ |> sc 3 |> tr { x = 1, y = -2 }
                    , { squ | col = Secondary } |> sc 3 |> tr { x = -2, y = -3 }
                    , squ |> sc 3 |> tr { x = -3, y = 0 }
                    , { squ | col = Secondary } |> sc 3 |> tr { x = 0, y = 1 }
                    ]
                , rotatable = False
                , bounds = ( { x = -3, y = -3 }, { x = 7, y = 7 } )
            }

        squareRule3 =
            { r
                | anchor = { squ | col = Ternary } |> rt { x = 0.5, y = 0.5 } 90
                , additions =
                    [ { squ | col = Secondary } |> sc 3 |> tr { x = 1, y = -2 }
                    , squ |> sc 3 |> tr { x = -2, y = -3 }
                    , { squ | col = Secondary } |> sc 3 |> tr { x = -3, y = 0 }
                    , squ |> sc 3 |> tr { x = 0, y = 1 }
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
    , open = [ squ |> sc 3 ]
    , closed = []
    , size = 10
    , start = { x = 0.5, y = 0.5 }
    }


trithagoreanTessellation : Tess
trithagoreanTessellation =
    let
        triRule1 =
            { r
                | anchor = eqi |> sc 3
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
            { r
                | anchor = { eqi | col = Ternary } |> rto 60
                , additions =
                    [ { eqi | col = Secondary } |> sc 2 |> rto 60 |> tr { x = -2, y = 0 }
                    , { eqi | col = Secondary } |> sc 2 |> rto 60 |> tr (equilateral |> setRotation 60 |> getPoint 1)
                    , { eqi | col = Secondary } |> sc 2 |> rto 60 |> tr (equilateral |> setRotation 60 |> getPoint 2) |> tr (equilateral |> setRotation 60 |> getPoint 1 |> mul 2 |> neg)
                    ]
                , rotatable = False
                , bounds = ( { x = -1.7, y = -2.3 }, { x = 4, y = 4 } )
            }

        triRule3 =
            { r
                | anchor = { eqi | col = Secondary } |> sc 2 |> rto 60
                , additions =
                    [ eqi |> sc 3
                    , eqi |> sc 3 |> tr (equilateral |> setRotation 60 |> getPoint 1 |> mul 2) |> tr { x = -3, y = 0 }
                    , eqi |> sc 3 |> tr (equilateral |> setRotation 60 |> getPoint 1 |> mul 2) |> tr (equilateral |> getPoint 2 |> neg)
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
    , open = [ eqi |> sc 3 ]
    , closed = []
    , size = 15
    , start = { x = 0.5, y = 0.5 }
    }


hexaStarTessellation : Tess
hexaStarTessellation =
    let
        hx =
            hexv |> sz 2

        hexRule1 =
            { r
                | anchor = hexv |> sc 2
                , additions =
                    [ { eqi | col = Ternary } |> rto 60
                    , { eqi | col = Ternary } |> tr (hx |> pt 1)
                    , { eqi | col = Ternary } |> rto -60 |> tr (hx |> pt 2)
                    , { eqi | col = Ternary } |> rto -120 |> tr (hx |> pt 3)
                    , { eqi | col = Ternary } |> rto -180 |> tr (hx |> pt 4)
                    , { eqi | col = Ternary } |> rto 120 |> tr (hx |> pt 5)
                    ]
                , bounds = ( { x = -1.2, y = -0.5 }, { x = 5, y = 5 } )
            }

        eq1 =
            { eqi | col = Ternary } |> rto 180

        triRule1 =
            { r
                | anchor = eq1
                , additions =
                    [ { hexv | col = Secondary } |> sc 2 |> tr (eq1 |> pt 1) ]
                , bounds = ( { x = -2.2, y = -0.5 }, { x = 5, y = 5 } )
            }

        hexRule2 =
            { r
                | anchor = { hexv | col = Secondary } |> sc 2
                , additions =
                    [ hexv |> sc 2 |> tr { x = -1, y = 0 } |> tr (pt 4 hx)
                    , { eqi | col = Quart } |> rto -60 |> tr (hx |> pt 2)
                    ]
                , bounds = ( { x = -1.2, y = 0 }, { x = 5, y = 7 } )
            }
    in
    { rules =
        [ hexRule1
        , triRule1
        , hexRule2
        ]
    , open = [ hexv |> rto 0 |> sc 2 ]
    , closed = []
    , size = 15
    , start = { x = 0.5, y = 0.5 }
    }


hexaGyraTessellation : Tess
hexaGyraTessellation =
    let
        hexRule =
            { r
                | anchor = hexv
                , additions =
                    [ { eqi | col = Secondary } |> rto 60 |> sc 2
                    , { eqi | col = Ternary } |> tr (pt 1 hexv) |> sc 2
                    , { eqi | col = Secondary } |> rto -60 |> tr (pt 2 hexv) |> sc 2
                    , { eqi | col = Ternary } |> rto -120 |> tr (pt 3 hexv) |> sc 2
                    , { eqi | col = Secondary } |> rto -180 |> tr (pt 4 hexv) |> sc 2
                    , { eqi | col = Ternary } |> rto -240 |> tr (pt 5 hexv) |> sc 2
                    ]
                , bounds = ( { x = -2, y = -1.5 }, { x = 5, y = 5 } )
            }

        triRule1 =
            { r
                | anchor = { eqi | col = Secondary } |> rto 60 |> sc 2
                , additions =
                    [ hexv |> tr (eqi |> rto 60 |> sz 2 |> pt 1) |> tr { x = 1, y = 0 } ]
                , bounds = ( { x = -1, y = -3 }, { x = 5, y = 5 } )
            }

        triRule2 =
            { r
                | anchor = { eqi | col = Ternary } |> sc 2
                , additions =
                    [ hexv |> tr (eqi |> sz 2 |> pt 1) |> tr (hexv |> pt 5) ]
                , bounds = ( { x = -1, y = 0 }, { x = 5, y = 5 } )
            }
    in
    { rules =
        [ hexRule
        , triRule1
        , triRule2
        ]
    , open = [ hexv |> rto 0 ]
    , closed = []
    , size = 20
    , start = { x = 0.5, y = 0.5 }
    }
