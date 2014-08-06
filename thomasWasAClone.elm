--Collisions/jumping working!
import Char
import Dict
import Keyboard
import Random
import Window

--CONVENIENCE FUNCTIONS
--elem : (Eq a) => a -> [a] -> Bool
elem x xs = any (\y -> y==x) xs

listToMaybe : [a] -> Maybe a
listToMaybe x = if | x == []   -> Nothing
                   | otherwise -> Just (head x)

--INPUTS
type Input = { xDir:Int, yDir:Int, shift:Bool, key1:Bool, key2:Bool, delta:Time }

delta = lift (\t -> t/20) (fps 60)

input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ Keyboard.shift
                               ~ Keyboard.isDown (Char.toCode '1')
                               ~ Keyboard.isDown (Char.toCode '2')
                               ~ delta)

--MODEL
type Player   = { x:Float, y:Float, vx:Float, vy:Float, w:Float, h:Float, objFill:Color }
type Obstacle = { x:Float, y:Float, w:Float, h:Float, objFill:Color }
type Game     = { activePlayer:Player, characters:Dict.Dict Int Player, obstacles:[Obstacle], colliding:Obstacle, level:Int }

defaultGame : Game
defaultGame = { activePlayer = player1
              , characters   = Dict.fromList [(1,player1), (2,player2)]
              , obstacles    = [ mapFloor
                               , { x=(-100), y=100, w=30, h=20, objFill=yellow }
                               , { x=65, y=70, w=150, h=15, objFill=purple}
                               , { x=-150, y=150, w=50, h=15, objFill=brown}
                               ]
              , colliding    = mapFloor
              , level = 1
              }
mapFloor : Obstacle
mapFloor = { x=0, y=0, w=600, h=50, objFill=lightGreen }
player1 : Player
player1 =  { x=-290, y=0, vx=0, vy=0, w=10, h=50, objFill=red }
player2 : Player
player2 = { x=290, y=0, vx=0, vy=0, w=25, h=25, objFill=blue }

--UPDATE
collision : Player -> Obstacle -> Bool
collision p obj = (p.x >= (obj.x - obj.w/2 - p.w/2) && 
                  p.x <= (obj.x + obj.w/2 + p.w/2)) && 
                  (p.y - p.h >= (obj.y - obj.h/2 - p.h/2) && 
                  p.y <= (obj.y + obj.h/2 + p.h/2))

setColliding : Player -> [Obstacle] -> Obstacle -> Obstacle
setColliding p os c = 
      let collider = listToMaybe . filter (collision p) <| os
      in maybe c (\x -> x) collider

setActive : Input -> Game -> Game
setActive i g = let pDefault = player1
                in if i.key1 then { g | activePlayer <- Dict.getOrElse pDefault 1 g.characters }
                else if i.key2 then { g | activePlayer <- Dict.getOrElse pDefault 2 g.characters }
                  else g

jump    y o p   = if y > 0 && (p.y <= (o.y + o.h/2 + p.h/2 )) 
                  then {p | vy <- 5} else p

gravity t o p   = if (p.y > (o.y + o.h/2 + p.h/2))
                  then {p | vy <-  p.vy - t/4}  else p

physics t o p = {p | x <- clamp (-halfWidth) (halfWidth) <| p.x + t * p.vx * 2
                   , y <- max (p.y + t*p.vy) <| (o.y + o.h/2 + p.h/2) 
                           
                }

walk    i p   = {p | vx <- if | i.shift   -> toFloat i.xDir*2
                              | otherwise -> toFloat i.xDir
                }

step i g = let oColl = setColliding g.activePlayer g.obstacles g.colliding
           in physics i.delta oColl . walk i . gravity i.delta oColl . jump i.yDir oColl <| g.activePlayer

stepGame : Input -> Game -> Game
stepGame i g = let g' = setActive i g
               in { g | activePlayer <- step i g' } 

gameState = foldp stepGame defaultGame input

--DISPLAY
(mainHeight, mainWidth) = (600, 800)
(halfHeight, halfWidth) = (mainHeight / 2, mainWidth /2)

make obj =
  rect obj.w obj.h |> filled obj.objFill 
                   |> move (obj.x, obj.y - halfHeight)
        
makeObstacle obj =
  rect obj.w obj.h |> filled obj.objFill
                   |> move (obj.x, obj.y - halfHeight)

display : (Int, Int) -> Game -> Element
display (w, h) g =
    container w h middle <| collage mainWidth mainHeight <|
         concat [
         [ rect mainWidth mainHeight |> filled lightBlue
         , asText "Thomas Was a Clone: Experimenting in Elm, inspired by Thomas Was Alone" |> toForm
                                                                                           |> move (0, 200)
         , asText g.activePlayer |> toForm |> move (0,100)
         , asText g.characters |> toForm |> move (0, 50)
         , make g.activePlayer
         ], (map make <| Dict.values g.characters),(map makeObstacle g.obstacles)]
      

main = lift2 display Window.dimensions gameState

