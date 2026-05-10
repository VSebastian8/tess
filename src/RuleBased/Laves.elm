module RuleBased.Laves exposing (..)

import Polygon exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import Util exposing (..)


lavesTesselations : List ( String, Tess )
lavesTesselations =
    [ ( "Triakis Triangular", triakisTriangularTessellation )
    , ( "Rhombile", rhombileTessellation )
    , ( "Tetrakis Square", tetrakisSquareTessellation )
    , ( "Disdyakis Rhombile", disdyakisRhombileTessellation )
    ]


triakisTriangularTessellation : Tess
triakisTriangularTessellation =
    let
        triTile1 =
            { r
                | anchor = eqi
                , additions =
                    [ { eqi | col = Secondary } |> rto -60 |> tr (eqi |> pt 1)
                    , { eqi | col = Secondary } |> rto -60
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

        subRule1 =
            { r
                | anchor = bigT
                , additions =
                    [ ois |> rto 150 |> tr bigT.centre
                    , { ois | col = Secondary } |> rto 30 |> tr bigT.centre
                    , { ois | col = Ternary } |> rto -90 |> tr bigT.centre
                    ]
                , bounds = ( { x = 0, y = 0 }, { x = 1.7, y = 1 } )
                , subdivide = True
                , rotatable = False
            }

        subRule2 =
            { r
                | anchor = { bigT | col = Secondary } |> rto -60
                , additions =
                    [ ois |> rto 90 |> tr (bigT |> rto -60).centre
                    , { ois | col = Secondary } |> rto -30 |> tr (bigT |> rto -60).centre
                    , { ois | col = Ternary } |> rto -150 |> tr (bigT |> rto -60).centre
                    ]
                , bounds = ( { x = -(cos (degrees 30)), y = 0 }, { x = 1.7, y = 1 } )
                , subdivide = True
                , rotatable = False
            }
    in
    { rules =
        [ triTile1
        , triTile2
        , subRule1
        , subRule2
        ]
    , closed = []
    , open = [ eqi ]
    , size = 60
    , start = { x = 0.5, y = 0.5 }
    }


rhombileTessellation : Tess
rhombileTessellation =
    let
        hexTile =
            { r
                | anchor = hex
                , additions =
                    [ hex |> tr (hex |> pt 4 |> neg)
                    , hex |> tr (hex |> pt 1) |> tr { x = 0, y = -1 }
                    , hex |> tr (hex |> pt 2)
                    , hex |> tr (hex |> pt 4)
                    , hex |> tr (hex |> pt 1 |> neg) |> tr { x = 0, y = 1 }
                    , hex |> tr (hex |> pt 2 |> neg)
                    ]
                , bounds = ( { x = -1.6, y = -2 }, { x = 5, y = 5 } )
            }

        subRule1 =
            { r
                | anchor = hex
                , additions =
                    [ rho |> rto 90 |> tr (hex |> pt 5)
                    , { rho | col = Ternary } |> rto -30 |> tr (hex |> pt 1)
                    , { rho | col = Secondary } |> rto 210 |> tr (hex |> pt 3)
                    ]
                , bounds = ( { x = 0, y = 0 }, { x = 1.7, y = 1 } )
                , subdivide = True
                , rotatable = False
            }
    in
    { rules =
        [ hexTile
        , subRule1
        ]
    , closed = []
    , open =
        [ hex
        ]
    , size = 30
    , start = { x = 0.5, y = 0.5 }
    }


tetrakisSquareTessellation : Tess
tetrakisSquareTessellation =
    let
        sqTile =
            { r
                | anchor = squ
                , additions =
                    [ squ |> tr { x = 0, y = -1 }
                    , squ |> tr { x = 1, y = -1 }
                    , squ |> tr { x = 1, y = 0 }
                    , squ |> tr { x = 1, y = 1 }
                    , squ |> tr { x = 0, y = 1 }
                    , squ |> tr { x = -1, y = 1 }
                    , squ |> tr { x = -1, y = 0 }
                    , squ |> tr { x = -1, y = -1 }
                    ]
                , bounds = ( { x = -1, y = -1 }, { x = 3, y = 3 } )
            }

        subRule1 =
            { r
                | anchor = squ |> sc (sqrt 2)
                , additions =
                    [ iso |> rto -135 |> tr (squ |> sc (sqrt 2)).centre
                    , { iso | col = Ternary } |> rto 135 |> tr (squ |> sc (sqrt 2)).centre
                    , { iso | col = Secondary } |> rto 45 |> tr (squ |> sc (sqrt 2)).centre
                    , { iso | col = Quart } |> rto -45 |> tr (squ |> sc (sqrt 2)).centre
                    ]
                , bounds = ( { x = 0, y = 0 }, { x = sqrt 2, y = sqrt 2 } )
                , subdivide = True
                , rotatable = False
            }
    in
    { rules =
        [ sqTile
        , subRule1
        ]
    , closed = []
    , open =
        [ squ
        ]
    , size = 40
    , start = { x = 0.5, y = 0.5 }
    }


disdyakisRhombileTessellation : Tess
disdyakisRhombileTessellation =
    let
        hexTile =
            { r
                | anchor = hex
                , additions =
                    [ hex |> tr (hex |> pt 4 |> neg)
                    , hex |> tr (hex |> pt 1) |> tr { x = 0, y = -1 }
                    , hex |> tr (hex |> pt 2)
                    , hex |> tr (hex |> pt 4)
                    , hex |> tr (hex |> pt 1 |> neg) |> tr { x = 0, y = 1 }
                    , hex |> tr (hex |> pt 2 |> neg)
                    ]
                , bounds = ( { x = -1.6, y = -2 }, { x = 5, y = 5 } )
            }

        subRule1 =
            { r
                | anchor = hex
                , additions =
                    [ rgt |> rto -30
                    , { lft | col = Secondary } |> rto 150 |> tr hex.centre
                    , { rgt | col = Ternary } |> rto -90 |> tr (hex |> pt 1)
                    , lft |> rto 90 |> tr hex.centre
                    , { rgt | col = Secondary } |> rto -150 |> tr (hex |> pt 2)
                    , { lft | col = Ternary } |> rto 30 |> tr hex.centre
                    , rgt |> rto 150 |> tr (hex |> pt 3)
                    , { lft | col = Secondary } |> rto -30 |> tr hex.centre
                    , { rgt | col = Ternary } |> rto 90 |> tr (hex |> pt 4)
                    , lft |> rto -90 |> tr hex.centre
                    , { rgt | col = Secondary } |> rto 30 |> tr (hex |> pt 5)
                    , { lft | col = Ternary } |> rto -150 |> tr hex.centre
                    ]
                , bounds = ( { x = 0, y = 0 }, { x = 1.7, y = 1 } )
                , subdivide = True
                , rotatable = False
            }
    in
    { rules =
        [ hexTile
        , subRule1
        ]
    , closed = []
    , open =
        [ hex
        ]
    , size = 40
    , start = { x = 0.5, y = 0.5 }
    }
