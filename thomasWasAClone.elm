--Collisions/jumping working!

import Keyboard
import Window

type Input = { xDir:Int, yDir:Int, delta:Time }

delta = lift (\t -> t/20) (fps 60)

input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ delta)

type Player   = { x:Float, y:Float, vx:Float, vy:Float, w:Float, h:Float }
type Obstacle = { x:Float, y:Float, w:Float, h:Float, objFill:Color }
type Game     = { player:Player, obstacles:[Obstacle], colliding:Obstacle }

defaultGame : Game
defaultGame = { player    = { x=-290, y=0, vx=0, vy=0, w=10, h=50 }
              , obstacles = [ { x=(-100), y=100, w=30, h=20, objFill=yellow }
                            , { x=65, y=75, w=150, h=15, objFill=purple}
                            , { x=0, y=0, w=600, h=50, objFill=gray }
                            ]
              , colliding = { x=0, y=0, w=600, h=50, objFill=gray }
              }

--UPDATE

--elem : (Eq a) => a -> [a] -> Bool
elem x xs = any (\y -> y==x) xs

listToMaybe : [a] -> Maybe a
listToMaybe x = if | x == []   -> Nothing
                   | otherwise -> Just (head x)

collision : Player -> Obstacle -> Bool
collision p obj = (p.x >= (obj.x - obj.w/2 - p.w/2) && 
                  p.x <= (obj.x + obj.w/2 + p.w/2)) && 
                  (p.y - p.h >= (obj.y - obj.h/2 - p.h/2) && 
                  p.y <= (obj.y + obj.h/2 + p.h/2))

setColliding : Player -> [Obstacle] -> Obstacle -> Obstacle
setColliding p os c = 
      let collider = listToMaybe . filter (collision p) <| os
      in maybe c (\x -> x) collider

jump    y o p   = if y > 0 && (p.y <= (o.y + o.h/2 + p.h/2 + 10)) 
                  then {p | vy <- 10} else p

gravity t o p   = if (p.y > (o.y + o.h/2 + p.h/2))
                  then {p | vy <-  p.vy - t/4}  else p

physics t o p = {p | x <- p.x + t * p.vx * 2
                   , y <- max (p.y + t*p.vy) <| (o.y + o.h/2 + p.h/2) 
                           
                }

walk    x p   = {p | vx <- toFloat x}

step i g = let oColl = setColliding g.player g.obstacles g.colliding
           in physics i.delta oColl . walk i.xDir . gravity i.delta oColl . 
              jump i.yDir oColl <| g.player

stepGame i g = { g | player <- step i g } 

gameState = foldp stepGame defaultGame input

--DISPLAY
(mainHeight, mainWidth) = (600, 600)
(halfHeight, halfWidth) = (mainHeight / 2, mainWidth /2)

make obj =
  rect obj.w obj.h |> filled red 
                   |> move (obj.x, obj.y - halfHeight)
        
makeObstacle obj =
  rect obj.w obj.h |> filled obj.objFill
                   |> move (obj.x, obj.y - halfHeight)

display : (Int, Int) -> Game -> Element
display (w, h) g =
    container w h middle <| collage mainWidth mainHeight <|
         concat [[rect mainWidth mainHeight |> filled lightBlue
         , make g.player
         , move (0, 100) <| toForm <| asText <| g.player
         , toForm <| asText <| g.colliding
         ], (map makeObstacle g.obstacles)]
      

main = lift2 display Window.dimensions gameState

