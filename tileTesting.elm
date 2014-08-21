{-Makes a tile-map based on the gameMap nested array.-}
import Keyboard
import Window
import Debug (log)

import Tpoulsen.Vectors (dotProduct, subtract, collision, collisionMTV, findVerticies)
import Tpoulsen.Lib (listToMaybe)

--INPUT

type Input = { x:Int, y:Int, delta:Time }

delta : Signal Time
delta = lift (\t -> t/2) (fps 60)

input : Signal Input
input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ delta)

--MODEL
data Surface = Water | Sand | Grass 
type Tile    = { walkability:Float, coords:(Float, Float), dimensions:(Float,Float)
               , verticies:[(Float,Float)], repr:Form }
type GameMap = [Tile]

type Game    = { level:GameMap, player:Player }
type Player  = { x:Float, y:Float, vx:Float, vy:Float, r:Float }

defaultGame = { level=defaultMap, player=defaultPlayer }

defaultPlayer : Player
defaultPlayer = { x=150, y=100, vx=1, vy=1, r=10 }
defaultMap  = makeMap 50 gameMap
gameMap : [[Int]]
gameMap = [ [1,1,1,1,1,1,1,1,1,1,1,1]
          , [2,1,2,2,2,1,2,2,2,2,2,2]
          , [2,1,2,2,2,2,2,2,2,2,2,2]
          , [1,2,2,2,2,2,2,2,2,2,1,2]
          , [2,2,2,2,2,2,2,2,1,1,1,2]
          , [1,2,2,1,1,1,1,1,1,1,1,2]
          , [1,2,2,1,1,1,1,1,1,1,1,2]
          , [2,2,2,2,2,2,2,2,2,2,2,2]
          , [2,2,2,2,2,2,2,2,2,2,2,3]
          , [1,2,2,2,2,1,1,2,2,2,3,3]
          , [2,2,2,1,1,1,1,2,3,3,3,3]
          , [1,2,1,1,1,1,1,3,3,3,3,3]
          ]

--UPDATE

{-
outOfBounds : Float -> Float -> Player -> Bool
outOfBounds halfW halfH o = 
    o.x + (o.w/2) > halfW || o.x - (o.w/2) < -halfW || 
    o.y + (o.h/2) > halfH || o.y - (o.h/2) < -halfH-100
-}

euclideanDist : (number,number) -> (number,number) -> Float
euclideanDist (x1,y1) (x2,y2) = sqrt <| (x2-x1)^2 + (y2-y1)^2

movingAway : Tile -> Player -> (Float, Float) -> Bool
movingAway t p (x,y) = 
    let dist  = euclideanDist (.coords t) (p.x, p.y)
        dist' = euclideanDist (.coords t) (p.x+x, p.y-y)
    in dist' < dist 

movingTowards : Input -> GameMap -> Player -> Tile
movingTowards i m p =
    let allCoords = map .coords m
        x' = p.x + (toFloat i.x)*p.r/2
        y' = p.y + (toFloat i.y)*p.r/2
    in fst . head . drop 1 . sortBy snd . zip m <| map (euclideanDist (x',y')) allCoords

standingOn : GameMap -> Player -> Tile
standingOn m p = 
    let allCoords = map .coords m
    in fst . head . sortBy snd . zip m <| map (euclideanDist (p.x,p.y)) allCoords


multisampleVerticies : Input -> Player -> [[(Float,Float)]]
multisampleVerticies i p = 
    let (ix', iy')   = (toFloat i.x, toFloat i.y)
        multiVerticies = (findVerticies 4 p.r 45)
        p0 = (p.x,p.y)
        p1 = (p.x+(ix'*p.r/4),   p.y-(iy'*p.r/4))
        p2 = (p.x+(ix'*p.r/2),   p.y-(iy'*p.r/2))
        p3 = (p.x+(ix'*p.r*3/4), p.y-(iy'*p.r*3/4))
        p4 = (p.x+(ix'*p.r),     p.y-(iy'*p.r))
        --p5 = (p.x+(ix'*p.r*1.5), p.y-(iy'*p.r*1.5))
        --p6 = (p.x+(ix'*p.r*2),   p.y-(iy'*p.r*2))
    in map multiVerticies <| reverse [p0,p1,p2,p3,p4]

multiSampleCollisions : Input -> GameMap -> Tile -> Player -> ((Float,Float),Float)
multiSampleCollisions i m t p = 
    let pVerticies = multisampleVerticies i p
        allCollisions = map (collisionMTV t.verticies) pVerticies
        --safeCollisions = filter (safeToMoveTo m p) allCollisions
    in allCollisions |> sortBy snd |> listToMaybe |> maybe ((0,0),0) id

{-
safeToMoveTo : GameMap -> Player -> ((Float,Float),Float) -> Bool
safeToMoveTo m p (((xMTV,yMTV),magMTV) as v)  = 
    let testMove = { p | x <- p.x + xMTV * magMTV 
                       , y <- p.y - yMTV * magMTV }
        possibleVerticies = (\x -> findVerticies 4 x.r 45 (x.x,x.y)) testMove
        unwalkable    = map (\x -> collision <| x.verticies) . filter (\x -> x.walkability == 0) <| m
    in not . and <| map (\x -> x possibleVerticies) unwalkable
-}
walk : Input -> GameMap -> Player -> Player
walk i m p =
      let t = standingOn m p
      in { p | vx <- t.walkability
             , vy <- t.walkability }

physics : Input -> GameMap -> Player -> Player
physics i m p = 
    let (ix',iy') = (toFloat i.x, toFloat i.y)
        ((xMTV,yMTV),magMTV) = log "physics multi" <| multiSampleCollisions i m n p
        t = standingOn m p
        n = movingTowards i m p
        towardsEachOther = (dotProduct (subtract (p.x,p.y) n.coords) (subtract (p.x+ix',p.y-iy') (0,0))) < 0 |> log "towardsEachOther"
    in if (t.walkability == 0 || n.walkability == 0) --&& towardsEachOther
       then { p | x <- log "x" <|  p.x + xMTV * magMTV + ix'*25 
                , y <- log "y" <|  p.y - yMTV * magMTV + iy'*25 }
       else { p | x <- p.x + ix' * p.vx
                , y <- p.y - iy' * p.vy }

stepPlayer : Input -> GameMap -> Player -> Player
stepPlayer i m p = 
    p |> physics i m . walk i m

stepGame : Input -> Game -> Game
stepGame i g = { g | player <- stepPlayer i g.level g.player }

gameState : Signal Game
gameState = foldp stepGame defaultGame input

(mainWidth, mainHeight) = (800, 600)
(originX, originY)      = (-(mainWidth/2), mainHeight/2)


--Reads nested-array defining the map and creates the map.
makeMap : Float -> [[Int]] -> GameMap
makeMap r m =
    concatMap (makeRow r) <| zip m [1..length m] 

makeRow : Float -> ([Int], Int) -> GameMap
makeRow r (row',n') = 
    let n = toFloat n'
        row = zip row' [1..length row'] 
        makeSquare = (\x -> case (fst x) of
                                1 -> makeTile Water (((toFloat <| snd x)*r),(n*r)) r
                                2 -> makeTile Grass (((toFloat <| snd x)*r),(n*r)) r
                                3 -> makeTile Sand  (((toFloat <| snd x)*r),(n*r)) r
                     ) 
    in map makeSquare row

makeTile : Surface -> (Float,Float) -> Float -> Tile
makeTile s (x,y) r =
  let (tileColor,walkable) = 
      case s of
          Water -> (blue,0)
          Sand  -> (brown,2)
          Grass -> (green,10)
  in
    { walkability = walkable
    , coords      = (x, y)
    , dimensions  = (r,r)
    , verticies   = findVerticies 4 r 45 (x, y)
    , repr        = rect r r |> filled tileColor
                             |> move (originX+x, originY-y)
    }

makePlayer : Player -> Form
makePlayer p =
    rect (2*p.r) (2*p.r) |> filled black
                         |> move (originX+p.x, originY-p.y)

makeProjections : Input -> Player -> [Form]
makeProjections i p =
    let (ix', iy')   = (toFloat i.x, toFloat i.y)
        p0  = (p.x,p.y)
        p1 = (p.x+(ix'*p.r/4),   p.y-(iy'*p.r/4))
        p2 = (p.x+(ix'*p.r/2),   p.y-(iy'*p.r/2))
        p3 = (p.x+(ix'*p.r*3/4), p.y-(iy'*p.r*3/4))
        p4 = (p.x+(ix'*p.r),     p.y-(iy'*p.r))
        p5 = (p.x+(ix'*p.r*2),   p.y-(iy'*p.r*2))
        make = (\(x,y) -> rect (2*p.r) (2*p.r) |> outlined (solid gray)
                                               |> move (originX+x, originY-y))
    in map make [p0,p1,p2,p3,p4]

display : (Int, Int) -> Game -> Input -> Element
display (w,h) g i =
    let playMap = defaultMap
        p = g.player
    in collage (mainWidth+100) (mainHeight+100) <|
        concat [ map .repr playMap
               , [makePlayer g.player]
               --, [findVerticies 4 p.r 45 (p.x,p.y) |> asText |> toForm  |> move (200,200) ]
               --, [standingOn g.level p |> .verticies |> asText |> toForm |> move (200,0)]
               --, [movingTowards i g.level p |> .verticies |> asText |> toForm |> move (200,-200)]
               , makeProjections i p
               --, [squareVerticies (p.x,p.y) (15,15) |> asText |> toForm ]
        --, [playMap |> asText |> toForm]
        ]

main = lift3 display Window.dimensions gameState input
