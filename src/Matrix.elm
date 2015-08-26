module Matrix
  ( Matrix, Cell
  , empty, repeat, matrix, square, rowVector, colVector
  , numbered, numbered1, fromList
  , get, set, update
  , getMany, setMany, setManyWith, updateMany, updateManyWith
  , toList, toIndexedList, flatten, flattenArray
  , map, indexedMap
  , compare, compareI, comm, diff
  , isEmpty, isSquare,  width, height, size
  , getRow, getCol, firstRow, lastRow, firstCol, lastCol
  , rows, cols
  , takeRows, takeCols, takeRowsEnd, takeColsEnd
  , dropRows, dropCols, dropRowsEnd, dropColsEnd
  , slice
  , transpose
  , filterRows, filterCols
  , compareCells, combineCells
  ) where

{-| A library for fast immutable rectangular matrices. The elements in a matrix must have the same type. The matrices are implemented on top of Elm's built-in arrays: `type alias Matrix a = Array (Array a)`. While all functions in this library preserve the rectangular property of the matrices, this isn't enforced, so make sure you manipulate the matrices using this API and not Array functions.

Geared towards 2D games.

The API is very unstable and will be changed and revamped many times. I am very happy to discuss and accept any ideas, suggestions, and pull requests on the GitHub page:

http://github.com/sindikat/elm-matrix

# Types
@docs Matrix, Cell

# Create matrices
@docs empty, repeat, matrix, square, rowVector, colVector, numbered, numbered1, fromList

# Get and set
@docs get, set, update

# Advanced get and set
@docs getMany, setMany, setManyWith, updateMany, updateManyWith

# Convert to other types
@docs toList, toIndexedList, flatten, flattenArray

# Map
@docs map, indexedMap

# Compare two matrices
@docs compare, compareI, comm, diff

# Matrix properties
@docs isEmpty, isSquare, width, height, size

# Get rows and columns
@docs getRow, getCol, firstRow, lastRow, firstCol, lastCol, rows, cols, takeRows, takeCols, takeRowsEnd, takeColsEnd, dropRows, dropCols, dropRowsEnd, dropColsEnd, slice

# Misc
@docs transpose, filterRows, filterCols

# Cells
@docs compareCells, combineCells
-}

import Array exposing (Array)
import Array.Extra
import Array.Experimental
import Maybe exposing (andThen)
import Maybe.Extra
import List.Experimental

{-| An ordered collection of elements of the same type, arranged into a rectangular matrix of `m` columns and `n` rows.
-}
type alias Matrix a = Array (Array a)


{-| A representation of a single matrix cell, together with its horizontal and vertical indices.
-}
type alias Cell a = (Int, Int, a)


{-| Create an empty matrix of size 0 × 0. Probably won't ever be used in code. If you are using empty matrices in your code, you probably doing something wrong.
-}
empty : Matrix a
empty = Array.empty


{-| Create an array with given sizes, filled with a default element

    repeat 4 2 0

will give back the matrix

    0 0 0 0
    0 0 0 0
-}
repeat : Int -> Int -> a -> Matrix a
repeat w h = Array.repeat h << Array.repeat w


-- NOTE: Maybe rename it as initialize as in Array.initialize?
{-| Initialize a new matrix of size m × n. Delegates to a function of
type (Int -> Int -> a) to determine the value of the cell, where first
Int argument is col index and second Int argument is row index.

    matrix 4 3 (\x y -> x + y*2 + 1)

will give back the matrix

    1 2 3 4
    2 4 6 8
-}
matrix : Int -> Int -> (Int -> Int -> a) -> Matrix a
matrix w h f =
  Array.initialize h (\y -> Array.initialize w (\x -> f x y))


{-| Create a square matrix of a certain size.

    square 2 (+)

will give back the matrix

    0 1
    1 2
-}
square : Int -> (Int -> Int -> a) -> Matrix a
square s f = matrix s s f


{-| Create a row vector from an array. A row vector is matrix of size 1 × n. That is, it is a matrix with a single row.

    rowVector (Array.fromList [1,2,3,4,5])

will give back the matrix

    1 2 3 4 5
-}
rowVector : Array a -> Matrix a
rowVector = Array.repeat 1


{-| Create a column vector from an array. A column vector is matrix of size m × 1. That is, it is a matrix with a single column.

    colVector (Array.fromList [1,2,3,4,5])

will give back the matrix

    1
    2
    3
    4
    5
-}
colVector : Array a -> Matrix a
colVector = Array.map (Array.repeat 1)


{-| Create a matrix, fill it with increasing numbers, starting from 0.

    numbered 5 5

will give back the matrix

     0  1  2  3  4
     5  6  7  8  9
    10 11 12 13 14
    15 16 17 18 19
    20 21 22 23 24
-}
numbered : Int -> Int -> Matrix Int
numbered w h =
  matrix w h (\x y -> x + y*w)


{-| Create a matrix, fill it with increasing numbers, starting from 1.

    numbered1 5 5

will give back the matrix

     1  2  3  4  5
     6  7  8  9 10
    11 12 13 14 15
    16 17 18 19 20
    21 22 23 24 25
-}
numbered1 : Int -> Int -> Matrix Int
numbered1 w h =
  matrix w h (\x y -> x + y*w + 1)


{-| Convert a list of list into a matrix.

    fromList [[0,1], [1,2]] == square 2 (+)
-}
fromList : List (List a) -> Matrix a
fromList = Array.map Array.fromList << Array.fromList


{-| Get the element at a particular location.
Return Nothing if index out of bounds.

    get 1 1 (square 2 (+)) == Just 2
    get 2 2 (square 2 (+)) == Nothing
-}
get : Int -> Int -> Matrix a -> Maybe a
get x y a = Array.Experimental.get y a `andThen` Array.Experimental.get x


{-| Set the element at a particular location and return new matrix.
Return the matrix unchanged if index is out of bounds.

    set 1 1 10 (square 2 (+)) == fromList [[0,1], [1,10]]
-}
set : Int -> Int -> a -> Matrix a -> Matrix a
set x y e a =
  let
    row = Array.Experimental.get y a
  in
    case row of
      Nothing -> a
      Just row -> Array.set y (Array.set x e row) a


{-| Update the element at a particular location using its current
value. Return matrix unchanged if index is out of bounds.

    update 1 1 ((*)2) (square 2 (+)) == fromList [[0,1], [1,4]]
    update 1 1 ((+)5) (square 3 (+)) == fromList [[0,1,2],[1,7,3],[2,3,4]]
-}
update : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
update x y f a =
  let
    value = get x y a
  in
    case value of
      Nothing -> a
      Just e -> set x y (f e) a


{-| Given list of indexes in a form of a tuple,
return list of objects in corresponding cells.

    getMany [(0,0),(1,1),(2,2)] (square 3 (+)) == [0,2,4]
-}
getMany : List (Int, Int) -> Matrix a -> List a
getMany is a = List.filterMap identity
               <| List.foldl (\(x,y) acc -> get x y a::acc) [] is


{-| Given list of indexes and list of objects,
update multiple cells of an array.

    setMany [(0,0),(1,1),(2,2)] (9,8,7) (square 3 (+))

will give back this matrix:

     9 1 2
     1 8 3
     2 3 7
-}
setMany : List (Int, Int) -> List a -> Matrix a -> Matrix a
setMany is os a =
  let
    step ((x,y),o) acc = set x y o acc
  in
    List.foldl step a (List.map2 (,) is os)


{-| Similar to setMany, but behaves differently. You provide a
function that returns indexes, a function that converts objects,
and a list of objects. The index-generating function is applied
to each object to generate indexes. Then the converting function
is applied to each object. Then converted objects are put into
the matrix.

This function is useful if you use records of game objects and you
want to generate a matrix of where these objects are on a map.

    type alias Object = {x:Int, y:Int, char:Char}
    type alias Model = List Object
    model : Model
    model = [{x=0,y=0,char='@'}, {x=1,y=1,char='@'}]
    setManyWith (\{x,y} -> (x,y)) (\{char} -> char) model (square 3 (\_ _ -> '.'))

will give back this matrix:

    @ . .
    . @ .
    . . .
-}
setManyWith : (b -> (Int, Int)) -> (b -> a) -> List b -> Matrix a -> Matrix a
setManyWith indexF convertF os a =
  setMany (List.map indexF os) (List.map convertF os) a


{-| Apply function at multiple indexes and return a new matrix.

    updateMany [(0,0),(1,1)] ((+)5) (square 2 (+)) == fromList [[5,1],[1,7]]
-}
updateMany : List (Int, Int) -> (a -> a) -> Matrix a -> Matrix a
updateMany is f a =
  let
    step (x,y) = update x y f
  in
    List.foldl step a is


-- TODO: Add a comprehensive example.
{-| Similar to updateMany. Takes a function of type `a -> b -> a`,
a list of indexes, and a list of objects. Combines an old object
under index with a new object using the function.
-}
updateManyWith : (a -> b -> a) -> List (Int, Int) -> List b -> Matrix a -> Matrix a
updateManyWith f is os a =
  let
    step ((x,y),o) acc = update x y (\e -> f e o) acc
  in
    List.foldl step a (List.map2 (,) is os)


{-| Create a list of lists from a matrix

    toList (square 3 (+)) == [[0,1,2],[1,2,3],[2,3,4]]
-}
toList : Matrix a -> List (List a)
toList = List.map Array.toList << Array.toList


{-| Create a list of 3-tuples `(x,y,element)` from a matrix.

    toIndexedList (square 2 (+)) == [(0,0,0),(1,0,1),(0,1,1),(1,1,2)]
-}
toIndexedList : Matrix a -> List (Cell a)
toIndexedList = flatten << indexedMap (,,)


{-| Create a flat list from a matrix.

    flatten (square 3 (\x y -> x*2+y)) == [0,2,4,1,3,5,2,4,6]
-}
flatten : Matrix a -> List a
flatten = List.concat << toList


{-| Create a flat array from a matrix.

    flattenArray (square 3 (\x y -> x*2+y)) == Array.fromList [0,2,4,1,3,5,2,4,6]
-}
flattenArray : Matrix a -> Array a
flattenArray = Array.foldl Array.append Array.empty


{-| Apply a function to each element of a matrix.

    map ((+)5) (square 2 (+)) == fromList [[5,6], [6,7]]
-}
map : (a -> b) -> Matrix a -> Matrix b
map f = Array.map (Array.map f)


{-| Apply a function of type `Int -> Int -> a -> b` to each element
of a matrix. Int arguments of a function are col and row indexes
respectively.

    indexedMap (\x y e -> x) (square 3 (+)) == fromList [[0,1,2],[0,1,2],[0,1,2]]
-}
indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f =
  Array.indexedMap (\y -> Array.indexedMap (\x -> f x y))


{-| Take a binary function, a binary predicate, and 2 matrices. The 2 matrices are assumed equal size, although this is not enforced. Compare same-indexed elements using the binary predicate. Return the indexed list of all cells, for which the predicate returns True. The indexed list of cells is of the format `[(Int, Int, (a,b))]`, where `a` is type of element from first matrix, and `b` is type of element from second matrix.

Consider an example. Suppose you have two matrices:

    square 4 (+)  and  square 4 (*)
    -------            -------
    0 1 2 3            0 0 0 0
    1 2 3 4            0 1 2 3
    2 3 4 5            0 2 4 6
    3 4 5 6            0 3 6 9

We want to return the cells, where second matrix's element is greater than or equal first matrix's element. We want to return these cells in a list, with corresponding indexes.

    compare (<=) (square 4 (+)) (square 4 (*))

returns:

    [(0,0,(0,0)), (2,2,(4,4)), (3,2,(5,6)), (2,3,(5,6)), (3,3,(6,9))]

which corresponds to these cells in both matrices:

    square 4 (+)  and  square 4 (*)
    -------            -------
    0 . . .            0 . . .
    . . . .            . . . .
    . . 4 5            . . 4 6
    . . 5 6            . . 6 9
-}
compare : (a -> b -> Bool) -> Matrix a -> Matrix b -> List (Cell (a,b))
compare cmp a1 a2 =
  let
    il1 = toIndexedList a1
    il2 = toIndexedList a2

  in
    List.Experimental.filterMap2 combineCells (compareCells cmp) il1 il2


{-| Same as compare, but return only indexes of the form `[(Int, Int)]`, not triples with indexes and pair of elements of the form `[(Int,Int,(a,b))]`.

    compareI (<=) (square 4 (+)) (square 4 (*))

returns:

    [(0,0), (2,2), (3,2), (2,3), (3,3)]
-}
compareI : (a -> b -> Bool) -> Matrix a -> Matrix b -> List (Int, Int)
compareI cmp a1 a2 =
  let
    getIndexes (x,y,_) = (x,y)
  in
    List.map getIndexes <| compare cmp a1 a2


{-| Return a list of indices of those cells of matrix, which are equal. Named after GNU/Linux command `comm`.

    square 3 (+) `comm` square 3 always == [(0,0),(1,0),(2,0)]
-}
comm : Matrix a -> Matrix a -> List (Int, Int)
comm = compareI (==)


{-| Return a list of indices of those cells of matrix, which are *not* equal.

    square 3 (+) `diff` square 3 always == [(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)]
-}
diff : Matrix a -> Matrix a -> List (Int, Int)
diff = compareI (/=)


{-| Check if the matrix is empty, i.e. of size 0 × 0. Probably won't ever be used in code. If you are using empty matrices in your code, you probably doing something wrong.
-}
isEmpty : Matrix a -> Bool
isEmpty = (==) empty


{-| Check if matrix is square. That is, check that both width (number of columns) and height (number of rows) are equal.
-}
isSquare : Matrix a -> Bool
isSquare a = width a == height a


{-| Return width (number of columns) of a matrix.
-}
width : Matrix a -> Int
width a =
  let
    row = Array.Experimental.get 0 a
  in
    case row of
      Nothing -> 0
      Just row -> Array.length row


{-| Return height (number of rows) of a matrix.
-}
height : Matrix a -> Int
height a = Array.length a


{-| Return size of a matrix as a tuple (width, height).
-}
size : Matrix a -> (Int, Int)
size a = (width a, height a)


{-| Return a row of index *n* as a one-dimensional Array. Negative index means count from end. If index is out of bounds, return `Nothing`.
-}
getRow : Int -> Matrix a -> Maybe (Array a)
getRow = Array.Experimental.get


{-| Return a col of index *n* as a one-dimensional Array. Negative index means count from end. If index is out of bounds, return `Nothing`.
-}
getCol : Int -> Matrix a -> Maybe (Array a)
getCol n = Maybe.Extra.combineArray << Array.map (Array.Experimental.get n)


{-| Return first row of a matrix as an Array.
-}
firstRow : Matrix a -> Maybe (Array a)
firstRow = getRow 0


{-| Return last row of a matrix as an Array.
-}
lastRow : Matrix a -> Maybe (Array a)
lastRow = getRow -1


{-| Return first column of a matrix as an Array.
-}
firstCol : Matrix a -> Maybe (Array a)
firstCol = getCol 0


{-| Return last column of a matrix as an Array.
-}
lastCol : Matrix a -> Maybe (Array a)
lastCol = getCol -1


{-| Return the matrix as an array of arrays, where inner arrays are rows of the matrix.

    rows (numbered1 3 3) == Array.fromList [[1,2,3],[4,5,6],[7,8,9]]
-}
rows : Matrix a -> Array (Array a)
rows = identity


{-| Return the matrix as an array of arrays, where inner arrays are columns of the matrix.

    cols (numbered1 3 3) == Array.fromList [[1,4,7],[2,5,8],[3,6,9]]
-}
cols : Matrix a -> Array (Array a)
cols = transpose


{-| Take rows from up. Negative argument means count from down. Below are examples  of expressions with returned matrices underneath.

    takeRows 2 (numbered1 3 3)

    1 2 3
    4 5 6

    takeRows -2 (numbered1 3 3)

    4 5 6
    7 8 9
-}
takeRows : Int -> Matrix a -> Matrix a
takeRows = Array.Extra.sliceUntil


{-| Take columns from left. Negative argument means count from right. Below are examples  of expressions with returned matrices underneath.

    takeCols 2 (numbered1 3 3)

    1 2
    4 5
    7 8

    takeCols -2 (numbered1 3 3)

    2 3
    5 6
    8 9
-}
takeCols : Int -> Matrix a -> Matrix a
takeCols n = Array.map (Array.Extra.sliceUntil n)


{-| Take columns from down. Negative argument means count from up. Below are examples  of expressions with returned matrices underneath.

    takeRowsEnd 2 (numbered1 3 3)

    4 5 6
    7 8 9

    takeRowsEnd -2 (numbered1 3 3)

    1 2 3
    4 5 6
-}
takeRowsEnd : Int -> Matrix a -> Matrix a
takeRowsEnd n = Array.Extra.sliceFrom (-n)


{-| Take columns from right. Negative argument means count from left. Below are examples  of expressions with returned matrices underneath.

    takeColsEnd 2 (numbered1 3 3)

    2 3
    5 6
    8 9

    takeColsEnd -2 (numbered1 3 3)

    1 2
    4 5
    7 8
-}
takeColsEnd : Int -> Matrix a -> Matrix a
takeColsEnd n = Array.map (Array.Extra.sliceFrom (-n))


{-|-}
dropRows : Int -> Matrix a -> Matrix a
dropRows = Array.Extra.sliceFrom


{-|-}
dropCols : Int -> Matrix a -> Matrix a
dropCols n = Array.map (Array.Extra.sliceFrom n)


{-|-}
dropRowsEnd : Int -> Matrix a -> Matrix a
dropRowsEnd n = Array.Extra.sliceUntil (-n)


{-|-}
dropColsEnd : Int -> Matrix a -> Matrix a
dropColsEnd n = Array.map (Array.Extra.sliceFrom (-n))


{-| Take a rectangular piece out of a matrix. The arguments are: horizontal-start, horizontal-end, vertical-start, vertical-end. The indexes are zero-based. The slice extracts from start including up to but not including end on both dimensions.

    slice 1 6 2 4  (numbered1 10 10)

will give back the matrix:

    22 23 24 25 26
    32 33 34 35 36

The slice supports negative arguments, which count from end.

    slice -9 -1 -4 -2 (numbered1 10 10)

will give back the matrix:

    62 63 64 65 66 67 68 69
    72 73 74 75 76 77 78 79
-}
slice : Int -> Int -> Int -> Int -> Matrix a -> Matrix a
slice wa we ha he a =
  Array.slice ha he a
    |> Array.map (Array.slice wa we)


-- TODO: Refactor
{-| Transpose a matrix.

    transpose (numbered1 2 4)

    1 3 5 7
    2 4 6 8
-}
transpose : Matrix a -> Matrix a
transpose a =
  let
    firstRow = getRow 0 a
    restRows = dropRows 1 a
  in
    case firstRow of
      Nothing -> a
      Just firstRow' ->
        let
          vector = colVector firstRow'
          step row acc = Array.Extra.map2 (Array.append) acc (colVector row)
        in
          Array.foldl step vector restRows


{-| Remove rows that do not satisfy predicate. The row in a predicate is represented as an `Array`.
-}
filterRows : (Array a -> Bool) -> Matrix a -> Matrix a
filterRows p = Array.filter p


{-| Remove cols that do not satisfy predicate. The col in a predicate is represented as an `Array`.
-}
filterCols : (Array a -> Bool) -> Matrix a -> Matrix a
filterCols p = transpose << Array.filter p << transpose


{-| Take a binary predicate and two cells. Compare the elements within these cells. That is return True, if the predicate returns True for these elements, otherwise return False. Ignore indices during comparison.

    compareCells (>=) (1,1,10) (1,1,5) == True
    compareCells (>=) (1,1,10) (2,3,5) == True
-}
compareCells : (a -> b -> Bool) -> Cell a -> Cell b -> Bool
compareCells cmp (_,_,e1) (_,_,e2) = e1 `cmp` e2


{-| Combine two cells by putting their elements into a tuple. Their indices are assumed to be equal.
-}
combineCells : Cell a -> Cell b -> Cell (a,b)
combineCells (x,y,e1) (_,_,e2) = (x,y,(e1,e2))
