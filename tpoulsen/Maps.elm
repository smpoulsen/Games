module Tpoulsen.Maps where

import Array
import Tpoulsen.Vectors (findVerticies, dotProduct, subtract)

data Surface = Water | Sand | Grass 
type Tile    = { walkability:Float, coords:(Float, Float), dimensions:Float
               , verticies:[(Float,Float)], repr:Form }
type GameMap = (Float, Array.Array (Array.Array Tile))

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
    (r, Array.fromList <| map (makeRow r) <| zip m [1..length m])

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
displayMap = concatMap id . Array.toList . Array.map (\x -> Array.toList <| Array.map .repr <| x) . snd

currentTile : GameMap -> (Float,Float) -> Tile
currentTile (r,m) (x,y) = 
    let tX = div (round x) (round r) |> clamp 0 ((Array.length m)-1)
        tY = div (round y) (round r) |> clamp 0 ((Array.length m)-1)
    in Array.getOrFail tX . Array.getOrFail tY <| m

surroundingTiles : GameMap -> (Float,Float) -> [Tile]
surroundingTiles (r,m) (x,y) = 
    let tX = div (round x) (round r) 
        tY = div (round y) (round r)
        (xMin,xMax)  = ((tX-1) |> clamp 0 (Array.length m), (tX+2) |> clamp 0 ((Array.length m)-1))
        (yMin,yMax)  = ((tY-1) |> clamp 0 (Array.length m), (tY+2) |> clamp 0 ((Array.length m)-1))
        sArray = Array.map (Array.slice xMin xMax) . Array.slice yMin yMax <| m
    in concatMap id . Array.toList . Array.map Array.toList <| sArray

--Map -> Position -> Movement vector -> Tile
movingTowards : GameMap -> (Float,Float) -> (Float,Float) -> [Tile]
movingTowards (r,m) (x,y) (vx,vy) =
    let towards     = (\t -> (dotProduct (subtract (x,y) t.coords) (subtract (x+vx,y-vy) (0,0))) < 0)
        surrounding = surroundingTiles (r,m) (x,y)
    in filter towards surrounding


--For degbugging.
(mainWidth, mainHeight) = ((50*12), (50*12))
(originX, originY)      = (-(mainWidth/2), mainHeight/2)