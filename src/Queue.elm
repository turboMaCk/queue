module Queue
    exposing
        ( Queue
        , empty
        , isEmpty
        , size
        , enqueue
        , dequeue
        , front
        , fromList
        , toList
        , map
        , filter
        )

{-| Simple FIFO (first in, first out) queue implementation with focus on simplicity.

# Type

@docs Queue, empty

# Query

@docs isEmpty, size, enqueue, dequeue, front

# Lists

@docs fromList, toList

# Transformations

@docs map, filter
-}

-- Types


type alias Rear a =
    List a


type alias Front a =
    List a


{-| -}
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


{-| Construct empty `Queue`
-}
empty : Queue a
empty =
    Queue [] []



-- Query


{-| Determine if queue is empty

```
Queue.isEmpty Queue.empty == True
Queue.isEmpty (Queue.fromList [ 1, 2 ]) == False
```
-}
isEmpty : Queue a -> Bool
isEmpty (Queue fl rl) =
    List.isEmpty fl && List.isEmpty rl


{-| Get size of `Queue`

```
Queue.size empty == 0
Queue.size (Queue.fromList [ 1, 2 ]) = 2
```
-}
size : Queue a -> Int
size (Queue fl rl) =
    List.length fl + List.length rl


{-| Add item to `Queue`

```
Queue.length (Queue.enqueue 1 Queue.empty) == 1
Queue.length (Queue.enqueue 1 (Queue.fromList [ 1, 2 ])) == 3
```
-}
enqueue : a -> Queue a -> Queue a
enqueue a (Queue fl rl) =
    queue fl <| a :: rl


{-| Take item from `Queue`

```
Queue.dequeue Queue.empty == ( Nothing, Queue.empty )
Queue.dequeue (Queue.fromList [ 1 ]) == ( Just 1, Queue.empty )
```
-}
dequeue : Queue a -> ( Maybe a, Queue a )
dequeue (Queue fl rl) =
    case fl of
        [] ->
            ( Nothing, Queue [] [] )

        head :: tail ->
            ( Just head, queue tail rl )


{-| Ask for front item without removing it from `Queue`

```
Queue.front Queue.empty == Nothing
Queue.front (Queue.fromList [ 1, 2 ]) == Just 1
-}
front : Queue a -> Maybe a
front (Queue fl _) =
    List.head fl



-- Lists


{-| Build `Queue` from `List`

```
Queue.fromList [] == Queue.empty
Queue.length (Queue.fromList [ 1, 2, 3 ])) == 3
```
-}
fromList : List a -> Queue a
fromList list =
    Queue list []


{-| Convert `Queue` to `List`

```
Queue.toList (Queue.fromList []) == []
Queue.toList (Queue.fromList [ 1, 2, 3]) == [ 1, 2, 3 ]
```
-}
toList : Queue a -> List a
toList (Queue fl rl) =
    fl ++ List.reverse rl



-- Transform


{-| Map function ofer `Queue`

```
Queue.toList (Queue.map identity (Queue.fromList [ 1, 2 ])) == [ 1, 2 ]
Queue.toList (Queue.map ((+) 1) (Queue.fromList [ 1, 2 ])) == [ 2, 3 ]
-}
map : (a -> b) -> Queue a -> Queue b
map fc (Queue fl rl) =
    let
        map_ =
            List.map fc
    in
        queue (map_ fl) (map_ rl)


{-| Filter items items in `Queue`

```
Queue.toList (Queue.filter identity (Queue.fromList [ True, False ])) == [ True ]
Queue.toList (Queue.filter ((<) 1) (Queue.fromList [ 1, 2 ])) == [ 2 ]
-}
filter : (a -> Bool) -> Queue a -> Queue a
filter fc (Queue fl rl) =
    let
        f =
            List.filter fc
    in
        queue (f fl) (f rl)
