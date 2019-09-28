module Lane exposing
    ( Lane (..)
    , LaneMap
    , fill
    , get
    , put
    , toList
    )

type Lane
    = Left
    | MiddleLeft
    | MiddleRight
    | Right

type LaneMap a =
    LaneMap { left : a, middleLeft : a, middleRight : a, right : a }

fill : a -> LaneMap a
fill e =
    LaneMap { left = e, middleLeft = e, middleRight = e, right = e }

get : Lane -> LaneMap a -> a
get lane (LaneMap map) =
    case lane of
        Left ->
            map.left

        MiddleLeft ->
            map.middleLeft

        MiddleRight ->
            map.middleRight

        Right ->
            map.right

put : Lane -> a -> LaneMap a -> LaneMap a
put lane e (LaneMap map) =
    case lane of
        Left ->
            LaneMap { map | left = e }

        MiddleLeft ->
            LaneMap { map | middleLeft = e }

        MiddleRight ->
            LaneMap { map | middleRight = e }

        Right ->
            LaneMap { map | right = e }

toList : LaneMap a -> List ( Lane, a )
toList (LaneMap map) =
    [ ( Left, map.left )
    , ( MiddleLeft, map.middleLeft )
    , ( MiddleRight, map.middleRight )
    , ( Right, map.right )
    ]
