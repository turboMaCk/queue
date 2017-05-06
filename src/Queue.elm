module Queue exposing (..)


type alias Rear a =
    List a


type alias Front a =
    List a


type Queue a
    = Queue (Front a) (Rear a)


{-| private pseudo-constructor
-}
queue : Front a -> Rear a -> Queue a
queue fl rl =
    case fl of
        [] ->
            Queue (List.reverse rl) []

        _ ->
            Queue fl rl


empty : Queue a
empty =
    Queue [] []


isEmpty : Queue a -> Bool
isEmpty (Queue fl rl) =
    List.isEmpty fl && List.isEmpty rl


size : Queue a -> Int
size (Queue fl rl) =
    List.length fl + List.length rl


enqueue : a -> Queue a -> Queue a
enqueue a (Queue fl rl) =
    queue fl <| a :: rl


dequeue : Queue a -> ( Maybe a, Queue a )
dequeue (Queue fl rl) =
    case fl of
        [] ->
            ( Nothing, Queue [] [] )

        head :: tail ->
            ( Just head, queue tail rl )


next : Queue a -> Maybe a
next (Queue fl _) =
    List.head fl


fromList : List a -> Queue a
fromList list =
    Queue list []


toList : Queue a -> List a
toList (Queue fl rl) =
    fl ++ List.reverse rl


map : (a -> b) -> Queue a -> Queue b
map fc (Queue fl rl) =
    let
        map_ =
            List.map fc
    in
        queue (map_ fl) (map_ rl)


filter : (a -> Bool) -> Queue a -> Queue a
filter fc (Queue fl rl) =
    let
        f =
            List.filter fc
    in
        queue (f fl) (f rl)
