module Tpoulsen.Maps where

import Array
import Tpoulsen.Vectors (findVerticies)

data Surface = Water | Sand | Grass 
type Tile    = { walkability:Float, coords:(Float, Float), dimensions:Float
               , verticies:[(Float,Float)], repr:Form }
type GameMap = Array.Array (Array.Array Tile)

gameMap : [[Int]]
gameMap = [ [1,2,2,1,1,2,2,1,2,1,1,2]
          , [2,1,2,2,2,1,2,2,2,2,2,2]
          , [2,1,2,2,2,1,2,2,2,2,2,1]
          , [1,2,2,2,2,2,2,2,1,2,1,1]
          , [2,2,2,1,2,2,2,2,1,2,1,2]
          , [1,2,2,1,1,1,1,1,1,1,2,2]
          , [1,2,2,1,1,1,1,1,1,1,1,2]
          , [2,2,2,2,2,2,2,2,2,2,2,2]
          , [2,2,2,2,2,1,2,2,2,2,2,1]
          , [1,2,2,2,2,1,1,2,3,2,1,1]
          , [2,2,2,1,1,2,2,2,3,2,3,3]
          , [1,2,1,1,2,1,1,2,3,3,2,2]
          ]

--Reads nested-list defining the map and creates the map.
makeMap : Float -> [[Int]] -> GameMap
makeMap r m =
    Array.fromList <| map (makeRow r) <| zip m [1..length m] 

makeRow : Float -> ([Int], Int) -> Array.Array Tile
makeRow r (row',n') = 
    let n = toFloat n'
        row = zip row' [1..length row'] 
        makeSquare = (\x -> case (fst x) of
                                1 -> makeTile Water (((toFloat <| snd x)*r),(n*r)) r
                                2 -> makeTile Grass (((toFloat <| snd x)*r),(n*r)) r
                                3 -> makeTile Sand  (((toFloat <| snd x)*r),(n*r)) r
                     ) 
    in Array.fromList <| map makeSquare row

makeTile : Surface -> (Float,Float) -> Float -> Tile
makeTile s (x,y) r =
  let vs = findVerticies 4 25 45 (x, y)
      (tileColor,walkable) = 
      case s of
          Water -> (blue,0)
          Sand  -> (brown,2)
          Grass -> (green,10)
  in
    { walkability = walkable
    , coords      = (x, y)
    , dimensions  = r
    , verticies   = vs
    , repr        = rect r r |> filled tileColor
                             |> move (originX+x, originY-y)
    }

displayMap : GameMap -> [Form]
displayMap = concatMap id . Array.toList . Array.map (\x -> Array.toList <| Array.map .repr <| x)

--For degbugging.
(mainWidth, mainHeight) = (800, 600)
(originX, originY)      = (-(mainWidth/2), mainHeight/2)