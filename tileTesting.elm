{-Makes a tile-map based on the gameMap nested array.-}
import Keyboard
import Window
import Debug (log)

import Tpoulsen.Vectors (collisionMTV, findVerticies)
import Tpoulsen.Lib (listToMaybe)

--INPUT

type Input = { x:Int, y:Int, delta:Time }

delta : Signal Time
delta = fps 60

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
defaultMap  = makeMap gameMap
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

--UPDATE

squareVerticies : (Float, Float) -> (Float, Float) -> [(Float, Float)]
squareVerticies (x,y) (w,h) = 
    let (hw, hh) = (w/2, h/2)
    in [(x-hw, y+hh), (x+hw,y+hh), (x+hw, y-hh), (x-hw,y-hh)]

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
        dist' = euclideanDist (.coords t) (p.x+x, p.y+y)
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
        --p5 = (p.x+(ix'*p.r*2),   p.y-(iy'*p.r*2))
    in map multiVerticies <| reverse [p0,p1,p2,p3,p4]

multiSampleCollisions : Tile -> [[(Float,Float)]] -> ((Float,Float),Float)
multiSampleCollisions t pVerticies =
    let allCollisions = map (collisionMTV t.verticies) pVerticies
    in allCollisions |> sortBy snd |>  listToMaybe |> maybe ((0,0),0) id

walk : Input -> Tile -> Player -> Player
walk i t p = 
      { p | vx <- t.walkability/5
          , vy <- t.walkability/5 }

physics : Input -> Tile -> Tile -> Player -> Player
physics i t n p = 
    let ((xMTV,yMTV),magMTV) = log "physics multi" <| multiSampleCollisions n <| multisampleVerticies i p
    in if n.walkability == 0 && magMTV /= 0
       then  { p | x <- p.x + xMTV * magMTV 
                 , y <- p.y + yMTV * magMTV }
        else { p | x <- p.x + (toFloat i.x) *p.vx, y <- p.y + (toFloat -i.y)*p.vy }

stepPlayer : Input -> GameMap -> Player -> Player
stepPlayer i m p = 
    let t = standingOn m p
        n = movingTowards i m p
    in p |> physics i t n . walk i t

stepGame : Input -> Game -> Game
stepGame i g = { g | player <- stepPlayer i g.level g.player }

gameState : Signal Game
gameState = foldp stepGame defaultGame input

(mainWidth, mainHeight) = (800, 600)
(originX, originY)      = (-(mainWidth/2), mainHeight/2)

makeMap : [[Int]] -> GameMap
makeMap m =
    concatMap makeRow <| zip m [1..length m] 

makeRow : ([Int], Int) -> GameMap
makeRow (r',n') = 
    let n = toFloat n'
        r = zip r' [1..length r'] 
        makeSquare = (\x -> case (fst x) of
                                1 -> makeTile Water (toFloat <| snd x*50) (n*50)
                                2 -> makeTile Grass (toFloat <| snd x*50) (n*50) 
                                3 -> makeTile Sand  (toFloat <| snd x*50) (n*50) 
                     ) 
    in map makeSquare r

makeTile : Surface -> Float -> Float -> Tile
makeTile s x y =
  let (tileColor,walkable) = case s of
                                  Water -> (blue,0)
                                  Sand  -> (brown,2)
                                  Grass -> (green,10)
  in
    { walkability = walkable
    , coords      = (x, y)
    , dimensions  = (50,50)
    , verticies   = findVerticies 4 25 45 (x, y)
    , repr        = rect 50 50 |> filled tileColor
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
        --p5 = (p.x+(ix'*p.r*2),   p.y-(iy'*p.r*2))
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
