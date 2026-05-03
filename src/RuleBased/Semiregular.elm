module RuleBased.Semiregular exposing (..)

import Polygon exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import Util exposing (..)


semiregularTesselations : List ( String, Tess )
semiregularTesselations =
    [ ( "Truncated Square", truncatedSquareTessellation )
    , ( "Truncated Hexagonal", truncatedHexagonalTessellation )
    , ( "Trihexagonal", triHexagonalTessellation )
    , ( "Rhombitrihexagonal", rhombiTriHexagonalTessellation )
    , ( "Truncated Trihexagonal", truncatedTriHexagonalTessellation )
    ]


truncatedSquareTessellation : Tess
truncatedSquareTessellation =
    let
        rule1 =
            { r
                | anchor = oct
                , additions =
                    [ { squ | col = Ternary } |> rto 45 |> tr { x = 1, y = 0 }
                    , { squ | col = Ternary } |> rto -135
                    ]
                , bounds = ( { x = -1.5, y = -0.5 }, { x = 4, y = 3 } )
            }

        rule2 =
            { r
                | anchor = oct
                , additions =
                    [ { oct | col = Secondary } |> tr (squ |> rto 45 |> pt 2) |> tr { x = 1, y = 0 }
                    , { oct | col = Secondary } |> tr (squ |> rto 45 |> pt 2 |> neg) |> tr { x = -1, y = 0 }
                    ]
                , bounds = ( { x = -2.8, y = 0 }, { x = 6.8, y = 3 } )
            }

        rule3 =
            { r
                | anchor = { oct | col = Secondary }
                , additions =
                    [ oct |> tr (squ |> rto 45 |> pt 2) |> tr { x = 1, y = 0 }
                    , oct |> tr (squ |> rto 45 |> pt 2 |> neg) |> tr { x = -1, y = 0 }
                    ]
                , bounds = ( { x = -2.8, y = 0 }, { x = 6.8, y = 3 } )
            }

        rule4 =
            { r
                | anchor = oct
                , additions =
                    [ { oct | col = Secondary } |> tr (oct |> pt 5)
                    , { oct | col = Secondary } |> tr (oct |> pt 5 |> neg)
                    ]
                , bounds = ( { x = -1, y = -2.2 }, { x = 3, y = 6.8 } )
            }
    in
    { rules =
        [ rule1
        , rule2
        , rule4
        , rule3
        ]
    , open = [ oct ]
    , closed = []
    , size = 15
    , start = { x = 0.5, y = 0.5 }
    }


truncatedHexagonalTessellation : Tess
truncatedHexagonalTessellation =
    let
        rule1 =
            { r
                | anchor = dod
                , additions =
                    [ { eqi | col = Ternary } |> rto 60 |> tr (dod |> pt 1)
                    , { eqi | col = Ternary } |> tr (dod |> pt 3)
                    , { eqi | col = Ternary } |> rto -60 |> tr (dod |> pt 5)
                    , { eqi | col = Ternary } |> rto -120 |> tr (dod |> pt 7)
                    , { eqi | col = Ternary } |> rto 180 |> tr (dod |> pt 9)
                    , { eqi | col = Ternary } |> rto 120 |> tr (dod |> pt 11)
                    ]
                , bounds = ( { x = -1.5, y = -1 }, { x = 5, y = 5 } )
            }

        rule2 =
            { r
                | anchor = { eqi | col = Ternary }
                , additions =
                    [ { dod | col = Secondary } |> rt dod.centre -90 |> tr { x = 1, y = 0 }
                    ]
                , bounds = ( { x = 0, y = -1 }, { x = 4, y = 5 } )
            }

        rule3 =
            { r
                | anchor = { dod | col = Secondary }
                , additions =
                    [ dod |> rt dod.centre 30 |> tr (sub (dod |> pt 1) (dod |> pt 8))
                    , { eqi | col = Quart } |> rto -30 |> tr (dod |> pt 4)
                    ]
                , bounds = ( { x = 0, y = -4 }, { x = 4, y = 8 } )
            }
    in
    { rules =
        [ rule1
        , rule2
        , rule3
        ]
    , open = [ dod ]
    , closed = []
    , size = 15
    , start = { x = 0.5, y = 0.5 }
    }


triHexagonalTessellation : Tess
triHexagonalTessellation =
    let
        ruleHex1 =
            { r
                | anchor = hexv
                , additions =
                    [ { eqi | col = Secondary } |> tr (hexv |> pt 1)
                    , { eqi | col = Secondary } |> rto -60 |> tr (hexv |> pt 2) |> sz 2 |> sc 0.5
                    , { eqi | col = Secondary } |> rto -120 |> tr (hexv |> pt 3)
                    , { eqi | col = Secondary } |> rto 180 |> tr (hexv |> pt 4) |> sz 2 |> sc 0.5
                    , { eqi | col = Secondary } |> rto 120 |> tr (hexv |> pt 5)
                    , { eqi | col = Secondary } |> rto 60 |> sz 2 |> sc 0.5
                    ]
                , bounds = ( { x = -2, y = -1 }, { x = 5, y = 4 } )
            }

        ruleTri1 =
            { r
                | anchor = { eqi | col = Secondary } |> rto 60 |> sz 2 |> sc 0.5
                , additions =
                    [ { hexv | col = Ternary } |> tr (eqi |> rto 60 |> pt 1 |> mul 2) ]
                , bounds = ( { x = -1, y = -1 }, { x = 4, y = 0 } )
            }

        ruleTri2 =
            { r
                | anchor = { eqi | col = Secondary } |> rto 60
                , additions =
                    [ { hexv | col = Quart } |> tr (eqi |> rto 60 |> pt 1 |> mul 2) ]
                , bounds = ( { x = -1, y = -1 }, { x = 4, y = 0 } )
            }

        ruleHex2 =
            { r
                | anchor = { hexv | col = Ternary }
                , additions =
                    [ hexv |> tr (hexv |> pt 3 |> neg)
                    , hexv |> tr (sub (hexv |> pt 2) (hexv |> pt 5))
                    ]
                , bounds = ( { x = -1.5, y = -2 }, { x = 5, y = 5 } )
            }
    in
    { rules = [ ruleHex1, ruleTri1, ruleTri2, ruleHex2 ]
    , open = [ hexv ]
    , closed = []
    , size = 30
    , start = { x = 0.5, y = 0.5 }
    }


rhombiTriHexagonalTessellation : Tess
rhombiTriHexagonalTessellation =
    let
        hexRule =
            { r | anchor = hexv, additions = [ { squ | col = Ternary } |> tr { x = 0, y = -1 } ], bounds = ( { x = -1, y = -1 }, { x = 3, y = 3 } ) }

        squRule1 =
            { r
                | anchor = { squ | col = Ternary }
                , additions =
                    [ { eqi | col = Secondary } |> rto -30 |> tr { x = 1, y = 0 }
                    , { eqi | col = Secondary } |> rto 30 |> tr (eqi |> rto 30 |> pt 1 |> neg)
                    ]
                , bounds = ( { x = -1, y = -1 }, { x = 3, y = 3 } )
            }

        triRule =
            { r
                | anchor = { eqi | col = Secondary }
                , additions =
                    [ { squ | col = Ternary } |> rto -30 |> tr { x = 1, y = 0 }
                    , { squ | col = Ternary } |> rto 30 |> tr (squ |> rto 30 |> pt 1 |> neg)
                    ]
                , bounds = ( { x = -1, y = -1 }, { x = 3, y = 3 } )
            }

        squRule2 =
            { r
                | anchor = { squ | col = Ternary }
                , additions = [ hexv |> tr (hexv |> pt 4 |> neg) ]
                , bounds = ( { x = -1, y = -2 }, { x = 3, y = 3 } )
            }
    in
    { rules = [ hexRule, squRule1, triRule, squRule2 ]
    , open = [ hexv ]
    , closed = []
    , size = 30
    , start = { x = 0.5, y = 0.5 }
    }


truncatedTriHexagonalTessellation : Tess
truncatedTriHexagonalTessellation =
    let
        dodRule =
            { r | anchor = dod, additions = [ { squ | col = Ternary } |> tr (dod |> pt 1) |> tr { x = 0, y = -1 } ], bounds = ( { x = -1, y = -1.5 }, { x = 5, y = 5 } ) }

        squRule1 =
            { r
                | anchor = { squ | col = Ternary }
                , additions =
                    [ { hex | col = Secondary } |> tr { x = 1, y = 0 }
                    , { hex | col = Secondary } |> rto 60 |> tr (hex |> pt 2 |> neg) |> tr { x = 0, y = 1 }
                    ]
                , bounds = ( { x = -2, y = -1 }, { x = 5, y = 3 } )
            }

        hexRule =
            { r
                | anchor = { hex | col = Secondary }
                , additions =
                    [ { squ | col = Ternary } |> rto -60 |> tr (hex |> pt 3)
                    , { squ | col = Ternary } |> tr { x = -1, y = 0 }
                    ]
                , bounds = ( { x = -2, y = 0 }, { x = 5, y = 3 } )
            }

        squRule2 =
            { r
                | anchor = { squ | col = Ternary }
                , additions =
                    [ dod |> tr ((dod |> pt 8) |> neg)
                    ]
                , bounds = ( { x = -2, y = -3 }, { x = 5, y = 4 } )
            }
    in
    { rules =
        [ dodRule
        , squRule1
        , hexRule
        , squRule2
        ]
    , open =
        [ dod
        , { squ | col = Ternary } |> tr (dod |> pt 1) |> tr { x = 0, y = -1 }
        , { squ | col = Ternary } |> rt squ.centre -90 |> rto -30 |> tr (dod |> pt 5)
        , { squ | col = Ternary } |> rt squ.centre -90 |> rto -150 |> tr (dod |> pt 9)
        ]
    , closed = []
    , size = 15
    , start = { x = 0.5, y = 0.5 }
    }
