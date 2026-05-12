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
    , ( "Deltoidal Trihexagonal", deltodailTriHexagonalTessellation )
    , ( "Cairo Pentagonal", cairoPentagonalTessellation )
    , ( "Floret Pentagonal", floretPentagonalTessellation )
    , ( "Prismatic Pentagonal", prismaticPentagonalTessellation )
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


deltodailTriHexagonalTessellation : Tess
deltodailTriHexagonalTessellation =
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
                    [ eqi |> tr (eqi |> pt 2) |> tr { x = -1, y = 0 }
                    , eqi |> tr (eqi |> pt 2 |> neg) |> tr { x = 1, y = 0 }
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
                    [ { kit | col = Secondary }
                    , { kit | col = Ternary } |> rto -120 |> tr (bigT |> pt 1)
                    , kit |> rto 120 |> tr (bigT |> pt 2)
                    ]
                , bounds = ( { x = 0, y = 0 }, { x = 1.7, y = 1 } )
                , subdivide = True
                , rotatable = False
            }

        subRule2 =
            { r
                | anchor = { bigT | col = Secondary } |> rto -60
                , additions =
                    [ kit |> rto -60
                    , { kit | col = Secondary } |> rto -180 |> tr (bigT |> rto -60 |> pt 1)
                    , { kit | col = Ternary } |> rto 60 |> tr (bigT |> rto -60 |> pt 2)
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


cairoPentagonalTessellation : Tess
cairoPentagonalTessellation =
    let
        rhomb1 =
            { r
                | anchor = car |> rto 90
                , additions =
                    [ { car | col = Quart } |> tr (car |> rto 90 |> pt 3)
                    , { car | col = Ternary } |> rto 180 |> tr (car |> rto 90 |> pt 3) |> tr (car |> pt 1)
                    , { car | col = Secondary } |> rto -90 |> tr (car |> rto 90 |> pt 2) |> tr (car |> rto 90 |> pt 4) |> tr (sub (car |> pt 2) (car |> pt 4))
                    ]
                , bounds = ( { x = 0, y = -1.8 }, { x = 3.5, y = 3 } )
            }

        rhomb2 =
            { r
                | anchor = { car | col = Secondary } |> rto 90
                , additions =
                    [ { car | col = Ternary } |> tr (car |> rto 90 |> pt 3)
                    , { car | col = Quart } |> rto 180 |> tr (car |> rto 90 |> pt 3) |> tr (car |> pt 1)
                    , car |> rto -90 |> tr (car |> rto 90 |> pt 2) |> tr (car |> rto 90 |> pt 4) |> tr (sub (car |> pt 2) (car |> pt 4))
                    ]
                , bounds = ( { x = 0, y = -1.8 }, { x = 3.5, y = 3 } )
            }

        leftR =
            { r
                | anchor = car |> rto 90
                , additions = [ { car | col = Secondary } |> rto 270 |> tr (car |> rto 90 |> pt 1) ]
                , bounds = ( { x = -1.25, y = -0.8 }, { x = 2.5, y = 1 } )
            }

        rightR =
            { r
                | anchor = { car | col = Secondary } |> rto 90
                , additions = [ car |> rto 270 |> tr (car |> rto 90 |> pt 1) ]
                , bounds = ( { x = -1.25, y = -0.8 }, { x = 2.5, y = 1 } )
            }

        upR =
            { r
                | anchor = { car | col = Ternary } |> rto 180
                , additions = [ car |> rto 90 |> tr (car |> rto 180 |> pt 3) ]
                , bounds = ( { x = -1, y = -2.5 }, { x = 2, y = 2.5 } )
            }

        downR =
            { r
                | anchor = { car | col = Quart } |> rto 180
                , additions = [ { car | col = Secondary } |> rto 90 |> tr (car |> rto 180 |> pt 3) ]
                , bounds = ( { x = -1, y = -2.5 }, { x = 2, y = 2.5 } )
            }
    in
    { rules = [ rhomb1, rhomb2, leftR, rightR, upR, downR ]
    , closed = []
    , open = [ car |> rto 90 |> tr { x = -1, y = 0 } ]
    , size = 20
    , start = { x = 0.5, y = 0.5 }
    }


floretPentagonalTessellation : Tess
floretPentagonalTessellation =
    let
        petal1 =
            { r
                | anchor = flr
                , additions = [ flr |> rto -60 ]
                , bounds = ( { x = -0.75, y = 0 }, { x = 3, y = 2.5 } )
            }

        petal2 =
            { r
                | anchor = { flr | col = Secondary }
                , additions = [ { flr | col = Secondary } |> rto -60 ]
                , bounds = ( { x = -0.75, y = 0 }, { x = 3, y = 2.5 } )
            }

        petal3 =
            { r
                | anchor = { flr | col = Ternary }
                , additions = [ { flr | col = Ternary } |> rto -60 ]
                , bounds = ( { x = -0.75, y = 0 }, { x = 3, y = 2.5 } )
            }

        next1 =
            { r
                | anchor = flr
                , additions =
                    [ { flr | col = Ternary } |> rto -120 |> tr (flr |> pt 2) |> tr (flr |> pt 1)
                    , { flr | col = Secondary } |> rto 120 |> tr (flr |> pt 4) |> tr (flr |> rto 120 |> pt 2 |> neg)
                    ]
                , rotatable = False
                , bounds = ( { x = 0, y = 0 }, { x = 4.5, y = 4.5 } )
            }

        next2 =
            { r
                | anchor = flr |> rto -120 |> tr (flr |> pt 2) |> tr (flr |> pt 1)
                , additions =
                    [ { flr | col = Ternary } |> rto 120 |> tr (flr |> pt 4) |> tr (flr |> rto 120 |> pt 2 |> neg)
                    , { flr | col = Secondary }
                    ]
                , rotatable = False
                , bounds = ( { x = 0, y = 0 }, { x = 4.5, y = 4.5 } )
            }

        next3 =
            { r
                | anchor = flr |> rto 120 |> tr (flr |> pt 4) |> tr (flr |> rto 120 |> pt 2 |> neg)
                , additions =
                    [ { flr | col = Ternary }
                    , { flr | col = Secondary } |> rto -120 |> tr (flr |> pt 2) |> tr (flr |> pt 1)
                    ]
                , rotatable = False
                , bounds = ( { x = 0, y = 0 }, { x = 4.5, y = 4.5 } )
            }

        add1 =
            { r
                | anchor = { flr | col = Secondary }
                , additions = [ flr |> rto 180 |> tr (flr |> pt 1) |> tr (flr |> pt 2) ]
                , rotatable = False
                , bounds = ( { x = 0, y = -1 }, { x = 4.5, y = 3 } )
            }

        add2 =
            { r
                | anchor = { flr | col = Secondary } |> rto 120
                , additions = [ flr |> rto 180 |> tr (flr |> pt 1) |> tr (flr |> pt 2) |> rto 120 ]
                , rotatable = False
                , bounds = ( { x = -2.2, y = -4.3 }, { x = 3, y = 4.5 } )
            }

        add3 =
            { r
                | anchor = { flr | col = Secondary } |> rto -120
                , additions = [ flr |> rto 180 |> tr (flr |> pt 1) |> tr (flr |> pt 2) |> rto -120 ]
                , rotatable = False
                , bounds = ( { x = -3.5, y = 0 }, { x = 3.5, y = 3.5 } )
            }
    in
    { rules = [ petal1, next1, petal2, next2, petal3, next3, add1, add2, add3 ]
    , open =
        [ flr
        , { flr | col = Ternary } |> rto -120 |> tr (flr |> pt 2) |> tr (flr |> pt 1)
        , { flr | col = Secondary } |> rto 120 |> tr (flr |> pt 4) |> tr (flr |> rto 120 |> pt 2 |> neg)
        ]
    , closed = []
    , start = { x = 0.5, y = 0.5 }
    , size = 15
    }


prismaticPentagonalTessellation : Tess
prismaticPentagonalTessellation =
    let
        prism1 =
            { r
                | anchor = pri
                , additions =
                    [ { pri | col = Quart } |> tr { x = -1, y = 0 }
                    , { pri | col = Quart } |> tr { x = 1, y = 0 }
                    ]
                , bounds = ( { x = -1, y = 0 }, { x = 3, y = 1.5 } )
            }

        prism2 =
            { r
                | anchor = { pri | col = Quart }
                , additions =
                    [ pri |> tr { x = -1, y = 0 }
                    , pri |> tr { x = 1, y = 0 }
                    ]
                , bounds = ( { x = -1, y = 0 }, { x = 3, y = 1.5 } )
            }

        prism3 =
            { r
                | anchor = { pri | col = Secondary }
                , additions =
                    [ { pri | col = Ternary } |> tr { x = -1, y = 0 }
                    , { pri | col = Ternary } |> tr { x = 1, y = 0 }
                    ]
                , bounds = ( { x = -1, y = 0 }, { x = 3, y = 1.5 } )
            }

        up1 =
            { r
                | anchor = pri
                , additions = [ { pri | col = Secondary } |> rto 180 |> tr { x = 1, y = 0 } ]
                , bounds = ( { x = 0, y = -1.5 }, { x = 1, y = 3 } )
            }

        down1 =
            { r
                | anchor = pri
                , additions = [ { pri | col = Secondary } |> rto 180 |> tr (pri |> pt 3) |> tr { x = 1, y = 1 } ]
                , bounds = ( { x = -0.3, y = -0.3 }, { x = 2, y = 3 } )
            }

        up2 =
            { r
                | anchor = { pri | col = Secondary }
                , additions = [ pri |> rto 180 |> tr { x = 1, y = 0 } ]
                , bounds = ( { x = 0, y = -1.5 }, { x = 1, y = 3 } )
            }

        down2 =
            { r
                | anchor = { pri | col = Secondary }
                , additions = [ pri |> rto 180 |> tr (pri |> pt 3) |> tr { x = 1, y = 1 } ]
                , bounds = ( { x = -0.3, y = -0.3 }, { x = 2, y = 3 } )
            }
    in
    { rules = [ prism1, prism2, prism3, up1, down1, up2, down2 ]
    , closed = []
    , open = [ pri ]
    , size = 25
    , start = { x = 0.5, y = 0.5 }
    }
