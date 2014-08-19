{-Makes a tile-map based on the gameMap nested array.-}
import Keyboard
import Window

import Tpoulsen.Vectors (collisionMTV)

--INPUT

type Input = { x:Int, y:Int, delta:Time }

delta : Signal Time
delta = lift (\t -> t/20) (fps 60)

input : Signal Input
input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ delta)

--MODEL
type Tile    = { walkability:Float, coords:(Float, Float), dimensions:(Float,Float)
               , verticies:[(Float,Float)], repr:Form }
type GameMap = [Tile]

type Game    = { level:GameMap, player:Player }
type Player  = { x:Float, y:Float, vx:Float, vy:Float, w:Float, h:Float }

defaultGame = { level=defaultMap, player=defaultPlayer }

defaultPlayer : Player
defaultPlayer = { x=100.0, y=-130.0, vx=1, vy=1, w=15, h=15 }
defaultMap  = makeMap gameMap
gameMap : [[Int]]
gameMap = [ [1,2,2,1,1,2,2,1,2,1,1,2]
          , [2,1,2,2,2,1,2,2,2,2,2,2]
          , [2,1,2,2,2,1,2,2,2,2,2,1]
          , [1,2,2,2,2,2,1,2,1,2,1,1]
          , [2,2,2,1,2,2,2,2,1,2,1,2]
          , [1,2,1,1,2,1,1,2,1,1,2,2]
          , [1,2,2,1,1,2,2,1,2,1,1,2]
          , [2,1,2,2,2,2,2,2,2,2,2,2]
          , [2,1,2,2,2,1,2,2,2,2,2,1]
          , [1,2,2,2,2,1,1,2,3,2,1,1]
          , [2,2,2,1,1,2,2,2,3,2,3,3]
          , [1,2,1,1,2,1,1,2,3,3,2,2]
          ]

--UPDATE

squareVerticies : (Float, Float) -> (Float, Float) -> [(Float, Float)]
squareVerticies (x,y) (w,h) = 
    let (hw, hh) = (w/2, h/2)
    in [(x-hw, y+hh), (x+hw,y+hh), (x+hw, y-hh), (x-hw,y-hh)]

--Calculate displacement vector to move player out of collision with un-walkable tiles.
pushBack : ((Float, Float),(Float, Float)) -> (Float, Float) -> (Float,Float)
pushBack ((x1,y1),(x2,y2)) (px,yx) = (0,0)

outOfBounds : Float -> Float -> Player -> Bool
outOfBounds halfW halfH o = 
    o.x + (o.w/2) > halfW || o.x - (o.w/2) < -halfW || 
    o.y + (o.h/2) > halfH || o.y - (o.h/2) < -halfH-100

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
        x' = p.x + (toFloat i.x)
        y' = p.y + (toFloat i.y)
    in fst . head . drop 1 . sortBy snd . zip m <| map (euclideanDist (x',y')) allCoords

standingOn : GameMap -> Player -> Tile
standingOn m p = 
    let allCoords = map .coords m
    in fst . head . sortBy snd . zip m <| map (euclideanDist (p.x,p.y)) allCoords

walk : Input -> Tile -> Player -> Player
walk i t p = 
    let playerVerticies      = squareVerticies (p.x,p.y) (p.w,p.h)
        ((xMTV,yMTV),magMTV) = collisionMTV t.verticies playerVerticies
    in if magMTV > 0 && t.walkability == 0
       then  { p | vx <- xMTV * magMTV 
                 , vy <- yMTV * magMTV }
        else { p | vx <- t.walkability/5
                 , vy <- t.walkability/5 }

physics : Input -> Tile -> Player -> Player
physics i t p = 
    { p | x <- p.x + (toFloat i.x)*p.vx
        , y <- p.y + (toFloat i.y)*p.vy }

stepPlayer : Input -> GameMap -> Player -> Player
stepPlayer i m p = 
    let t  = standingOn m p
    in p |> physics i t . walk i t

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
                                1 -> makeWater (toFloat <| snd x*50) (n*50)
                                2 -> makeGrass (toFloat <| snd x*50) (n*50) 
                                3 -> makeSand  (toFloat <| snd x*50) (n*50) 
                     ) 
    in map makeSquare r

makeWater : Float -> Float -> Tile
makeWater x y = 
    { walkability = 0
    , coords      = (originX+x, originY-y)
    , dimensions  = (50,50)
    , verticies   = squareVerticies (originX+x, originY-y) (50, 50)
    , repr        = rect 50 50 |> filled blue
                               |> move (originX+x, originY-y)
    }

makeGrass : Float -> Float -> Tile
makeGrass x y =
    { walkability = 10
    , coords      = (originX+x, originY-y)
    , dimensions  = (50,50)
    , verticies   = squareVerticies (originX+x, originY-y) (50, 50)
    , repr        = rect 50 50 |> filled green
                               |> move (originX+x, originY-y)
    }

makeSand : Float -> Float -> Tile
makeSand x y = 
    { walkability = 2
    , coords      = (originX+x, originY-y)
    , dimensions  = (50,50)
    , verticies   = squareVerticies (originX+x, originY-y) (50, 50)
    , repr        = rect 50 50 |> filled brown
                               |> move (originX+x, originY-y)
    }

makePlayer : Player -> Form
makePlayer p =
    rect p.h p.w |> filled black
                 |> move (p.x, p.y)

display : (Int, Int) -> Game -> Element
display (w,h) g =
    let playMap = defaultMap
    in collage (mainWidth+100) (mainHeight+100) <|
        concat [ map .repr playMap
               , [makePlayer g.player]
               , [squareVerticies (g.player.x, g.player.y) (g.player.w,g.player.h) |> asText |> toForm ]
        --, [playMap |> asText |> toForm]
        ]

main = lift2 display Window.dimensions gameState
