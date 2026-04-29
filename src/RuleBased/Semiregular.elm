module RuleBased.Semiregular exposing (..)

import ColorTheme exposing (..)
import Polygon exposing (..)
import Rules exposing (..)
import Shapes exposing (..)
import Util exposing (..)


semiregularTesselations : List ( String, Tess )
semiregularTesselations =
    [ ( "Truncated Square", truncatedSquareTiling )
    ]


truncatedSquareTiling : Tess
truncatedSquareTiling =
    let
        rule1 =
            { anchor = oct
            , additions =
                [ { squ | col = Ternary } |> rto 45 |> tr { x = 1, y = 0 }
                , { squ | col = Ternary } |> rto -135
                ]
            , rotatable = True
            , bounds = ( { x = -1.5, y = -0.5 }, { x = 4, y = 3 } )
            }

        rule2 =
            { anchor = oct
            , additions =
                [ { oct | col = Secondary } |> tr (squ |> rto 45 |> pt 2) |> tr { x = 1, y = 0 }
                , { oct | col = Secondary } |> tr (squ |> rto 45 |> pt 2 |> neg) |> tr { x = -1, y = 0 }
                ]
            , rotatable = True
            , bounds = ( { x = -2.8, y = 0 }, { x = 6.8, y = 3 } )
            }

        rule3 =
            { anchor = { oct | col = Secondary }
            , additions =
                [ oct |> tr (squ |> rto 45 |> pt 2) |> tr { x = 1, y = 0 }
                , oct |> tr (squ |> rto 45 |> pt 2 |> neg) |> tr { x = -1, y = 0 }
                ]
            , rotatable = True
            , bounds = ( { x = -2.8, y = 0 }, { x = 6.8, y = 3 } )
            }

        rule4 =
            { anchor = oct
            , additions =
                [ { oct | col = Secondary } |> tr (oct |> pt 5)
                , { oct | col = Secondary } |> tr (oct |> pt 5 |> neg)
                ]
            , rotatable = True
            , bounds = ( { x = -1, y = -2.2 }, { x = 3, y = 6.8 } )
            }
    in
    { rules =
        [ rule1
        , rule2
        , rule4
        , rule3
        ]
    , open = [ oct |> tr { x = 25, y = 25 } ]
    , closed = []
    , size = 15
    }
