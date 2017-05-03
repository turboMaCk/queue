module Queue exposing (..)


type alias Rear a =
    List a


type alias Front a =
    List a


type Queue a
    = Queue (Front a) (Rear a)


initialize : Queue a
initialize =
    Queue [] []


isEmpty : Queue a -> Bool
isEmpty (Queue fl rl) =
    List.isEmpty fl && List.isEmpty rl


enqueue : a -> Queue a -> ( Maybe a, Queue a )
enqueue a (Queue fl rl) =
    ( Nothing, Queue fl <| a :: rl )


dequeue : Queue a -> ( Maybe a, Queue a )
dequeue queue =
    let
        (Queue fl rl) =
            balance queue
    in
        case fl of
            [] ->
                ( Nothing, queue )

            head :: tail ->
                ( Just head, Queue tail rl )


balance : Queue a -> Queue a
balance ((Queue fl rl) as queue) =
    case fl of
        [] ->
            Queue (List.reverse rl) []

        _ ->
            queue


fromList : List a -> Queue a
fromList list =
    Queue list []


toList : Queue a -> List a
toList (Queue fl rl) =
    fl ++ List.reverse rl


size : Queue a -> Int
size (Queue fl rl) =
    List.length fl + List.length rl


map : (a -> b) -> Queue a -> Queue b
map fc (Queue fl rl) =
    let
        map_ =
            List.map fc
    in
        Queue (map_ fl) (map_ rl)


filter : (a -> Bool) -> Queue a -> Queue a
filter fc (Queue fl rl) =
    let
        f =
            List.filter fc
    in
        Queue (f fl) (f rl)
