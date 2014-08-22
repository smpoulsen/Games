{-Makes a tile-map based on the gameMap nested array.-}
import Keyboard
import Window
import Debug (log)

import Tpoulsen.Vectors (dotProduct, subtract, collision, collisionMTV, findVerticies)
import Tpoulsen.Lib (listToMaybe)
import Tpoulsen.Maps (..)

--INPUT
type Input = { x:Int, y:Int, delta:Time }

delta : Signal Time
delta = lift (\t -> t/3) (fps 60)

input : Signal Input
input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ delta)

--MODEL
type Game    = { level:GameMap, player:Player }
type Player  = { x:Float, y:Float, vx:Float, vy:Float, r:Float }

defaultGame = { level=defaultMap, player=defaultPlayer }

defaultPlayer : Player
defaultPlayer = { x=200, y=150, vx=1, vy=1, r=10 }
defaultMap  = makeMap 50 gameMap

gameMap : [[Int]]
gameMap = [ [1,1,1,1,1,1,1,1,1,1,1,1]
          , [1,1,2,2,2,1,2,2,2,2,2,2]
          , [1,1,2,2,2,2,2,2,2,2,2,2]
          , [1,2,2,2,2,2,2,2,2,2,1,2]
          , [1,2,2,2,2,2,2,2,1,1,1,2]
          , [1,2,2,1,1,1,1,1,1,1,1,2]
          , [1,2,2,1,1,1,1,1,1,1,1,2]
          , [1,2,2,2,2,2,2,2,2,2,2,2]
          , [1,2,2,2,2,2,2,2,2,2,2,3]
          , [1,2,2,2,2,1,1,2,2,2,3,3]
          , [1,2,2,1,1,1,1,2,3,3,3,3]
          , [1,1,1,1,1,1,1,3,3,3,3,3]
          ]

--UPDATE
euclideanDist : (number,number) -> (number,number) -> Float
euclideanDist (x1,y1) (x2,y2) = sqrt <| (x2-x1)^2 + (y2-y1)^2

multisampleVerticies : (Int,Int) -> Player -> [[(Float,Float)]]
multisampleVerticies (vx, vy) p = 
    let (vx', vy')     = (toFloat vx, toFloat vy)
        multiVerticies = (findVerticies 4 p.r 45)
        p0 = (p.x,p.y)
        p1 = (p.x+(vx'*p.r/4),   p.y-(vy'*p.r/4))
        p2 = (p.x+(vx'*p.r/2),   p.y-(vy'*p.r/2))
        p3 = (p.x+(vx'*p.r*3/4), p.y-(vy'*p.r*3/4))
        p4 = (p.x+(vx'*p.r),     p.y-(vy'*p.r))
        --p5 = (p.x+(vx'*p.r*1.5), p.y-(vy'*p.r*1.5))
        --p6 = (p.x+(vx'*p.r*2),   p.y-(vy'*p.r*2))
    in map multiVerticies <| reverse [p0,p1,p2,p3,p4]

multiSampleCollisions : Input -> GameMap -> [Tile] -> Player -> ((Float,Float),Float)
multiSampleCollisions i m ts p = 
    let pVerticies    = multisampleVerticies (i.x,i.y) p
        currentT      = currentTile m (p.x,p.y)
        allCollisions = concatMap (\t -> map (\p -> collisionMTV t.verticies p) pVerticies) <| filter (\t -> t.walkability == 0) ts
    in allCollisions |> sortBy snd |> reverse |> resolveCollision p ts

resolveCollision : Player -> [Tile] -> [((Float,Float),Float)] -> ((Float,Float),Float)
resolveCollision p ts vs = filter (testMove p ts) vs |> listToMaybe |> maybe ((0,0),0) id

testMove : Player -> [Tile] -> ((Float,Float),Float) -> Bool
testMove p ts ((xMTV,yMTV),magMTV) =
    let moved  = findVerticies 4 p.r 45 <| (p.x + xMTV * magMTV , p.y + yMTV * magMTV)
    in all (\t -> not . collision moved <| t.verticies) ts

walk : GameMap -> Player -> Player
walk m p =
    let t = currentTile m (p.x,p.y)
    in { p | vx <- t.walkability/5
           , vy <- t.walkability/5 }

physics : Input -> GameMap -> Player -> Player
physics i m p = 
    let (ix',iy') = (toFloat i.x, toFloat i.y)
        t       = currentTile m (p.x,p.y)
        towards = movingTowards m (p.x,p.y) (ix',iy')
        ((xMTV,yMTV),magMTV) = multiSampleCollisions i m towards p |> log ("resolution")
    in if magMTV /= 0
       then { p | x <- p.x + xMTV * magMTV  
                , y <- p.y + yMTV * magMTV }
       else { p | x <- p.x + ix' * p.vx
                , y <- p.y - iy' * p.vy }

stepPlayer : Input -> GameMap -> Player -> Player
stepPlayer i m p = 
    p |> physics i m . walk m

stepGame : Input -> Game -> Game
stepGame i g = { g | player <- stepPlayer i g.level g.player }

gameState : Signal Game
gameState = foldp stepGame defaultGame input

--DISPLAY
(mainWidth, mainHeight) = ((50*12), (50*12))
(originX, originY)      = (-(mainWidth/2), mainHeight/2)

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
    let playMap = displayMap defaultMap
        p = g.player
    in collage (mainWidth+100) (mainHeight+100) <|
        concat [ playMap
               , [makePlayer g.player]
               , makeProjections i p
        ]

main = lift3 display Window.dimensions gameState input
