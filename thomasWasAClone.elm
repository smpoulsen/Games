{-
A small platformer demo written to try game development, FRP, and Elm.
Inspired by Thomas Was Alone.

-}
import Char
import Keyboard
import Time
import Window

--CONVENIENCE FUNCTIONS
elem x xs = any (\y -> y==x) xs

listToMaybe : [a] -> Maybe a
listToMaybe x = if | x == []   -> Nothing
                   | otherwise -> Just (head x)

--INPUTS
type Input = { xDir:Int, yDir:Int, shift:Bool, key1:Bool, delta:Time }

delta = lift (\t -> t/20) (fps 60)

input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ Keyboard.shift
                               ~ (2 * millisecond) `Time.since` Keyboard.isDown (Char.toCode '1')
                               ~ delta)

--MODEL
data WinState     = Won | Lost | InPlay
type Player       = { x:Float, y:Float, vx:Float, vy:Float, w:Float, h:Float
                    , jumpingV:Float, objFill:Color, active:Bool, alive:Bool }
type Obstacle     = { x:Float, y:Float, w:Float, h:Float, objFill:Color }
type Game         = { characters:[Player], obstacles:[Obstacle], state:WinState }
type GameObject a = { a | x:Float, y:Float, w:Float, h:Float }

defaultGame = { characters   = [player1]
              , obstacles    = [ mapSky, mapFloor
                               , { x=0,    y=0,   w=300, h=50, objFill=lightGreen }
                               , { x=1300, y=0,   w=500, h=50, objFill=lightGreen }
                               , { x=-100, y=100, w=30,  h=20, objFill=yellow }
                               , { x=65,   y=60,  w=150, h=15, objFill=purple}
                               , { x=0,    y=200, w=75,  h=15, objFill=black}
                               , { x=200,  y=300, w=75,  h=15, objFill=darkGreen}
                               , { x=750,  y=300, w=75,  h=15, objFill=blue}
                               , { x=1000, y=150, w=200, h=15, objFill=purple}
                               , { x=500,  y=70, w=50,   h=15, objFill=purple}
                               ]
              , state = InPlay
              }

mapFloor = { x=-halfWidth, y=0, w= halfWidth, h=50, objFill=lightGreen }
death    = { x=-halfWidth, y=0, w= mainWidth, h=0, objFill=black }
mapSky   = { x=halfWidth-250, y=halfHeight, w=3*mainWidth, h=mainHeight, objFill=lightBlue }
player1  = { x=-390, y=50, vx=0, vy=0, w=10, h=50, jumpingV=8.0, 
              objFill=red, active=True, alive=True }
player2  = { x=90, y=0, vx=0, vy=0, w=25, h=25, jumpingV=5.0, 
             objFill=blue, active=False, alive=True }

--UPDATE

--Collision detection:
collision : Player -> GameObject a -> Bool
collision p obj = (p.x >= (obj.x - obj.w/2 - p.w/2) && 
                  p.x <= (obj.x + obj.w/2 + p.w/2)) && 
                  (p.y - p.h/2 >= (obj.y + obj.h/2) && 
                  p.y  - p.h/2 <= (obj.y + obj.h/2  + p.h/2))

walkingInto p obj = (p.x <= (obj.x - obj.w/2 - p.w/2) || p.x >= (obj.x + obj.w/2 + p.w/2)) &&
                    (obj.y `elem` [p.y-p.h/2..p.y+p.h/2])

setColliding i g = 
      let activeP  = g.characters |> getActive 
          collider = listToMaybe . filter (collision activeP) <| g.obstacles
      in maybe death (\x -> x) collider

--Active character
toggleActive t p = if t then { p | active <- not p.active } 
                   else p

isActive : Player -> Bool
isActive p = p.active 

getActive : [Player] -> Player
getActive ps = ps |> listToMaybe . filter (\x -> isActive x) 
                  |> maybe (head ps) (\x -> x) 

dead i p = if p.y <= 25 then { p | alive <- False }
           else p

clearedLevel ps s = 
    let activeP = getActive ps
    in if | activeP.x >= halfWidth-50 -> Won
          | not activeP.alive -> Lost
          | otherwise -> s

--Movement & Physics
jump y o p = if p.active && y > 0 && (p.y == (o.y + o.h/2 + p.h/2 ))
             then { p | vy <- p.jumpingV } 
             else p

gravity t o p = if p.y > (o.y + o.h/2 + p.h/2)
                then { p | vy <-  p.vy - t/4 }  
                else p

physics t o p  = {p | x <- clamp (-mainWidth) (mainWidth) <| p.x + t * p.vx * 2
                   , y <- max (p.y + t*p.vy) <| (o.y + o.h/2 + p.h/2) 
                }

walk i o s p = if p.active 
               then { p | vx <- if | p.x <= -halfWidth && i.xDir <= 0 -> 0
                                   | p.x >=  halfWidth && i.xDir >= 0 -> 0
                                   | walkingInto p o -> 0
                                   | not p.alive || s == Won -> 0
                                   | i.shift     -> toFloat i.xDir * 2
                                   | otherwise   -> toFloat i.xDir
                    }
             else { p | vx <- 0 }

translate i p o = if p.active 
                  then { o | x <- o.x - i.delta * p.vx * 2 |> clamp (o.x-halfWidth) (o.x+halfWidth) }
                  else o

moveObjects i g = let activeP = g.characters |> getActive
                  in map (translate i activeP) g.obstacles

--Composed function that rolls the above together.
step : Input -> Game -> [Player] 
step i g = let oColl   = setColliding i g 
           in g.characters |> map (\x -> dead i . physics i.delta oColl . 
                                         walk i oColl g.state . gravity i.delta oColl . 
                                         jump i.yDir oColl . toggleActive i.key1  <| x )

stepGame i g = { g | characters <- step i g
                   , obstacles  <- moveObjects i g
                   , state      <- clearedLevel g.characters g.state
               }

gameState = foldp stepGame defaultGame input

--DISPLAY
(mainHeight, mainWidth) = (600, 1000)
(halfHeight, halfWidth) = (mainHeight / 2, mainWidth /2)

make p = 
  if p.alive then rect p.w p.h |> filled p.objFill
                                     |> move (p.x, p.y - halfHeight)
  else rect 0 0 |> filled gray
                |> move (p.x, p.y - halfHeight)    

makeObstacle p obj  =
  rect obj.w obj.h |> filled obj.objFill
                   |> move (obj.x, obj.y - halfHeight )
  
administrivia p s = 
  group [ toForm [markdown|Thomas Was a Clone: Experimenting in Elm, 
                 inspired by Thomas Was Alone|] |> move (-400-p.x,150)
        , toForm [markdown|&larr; move &rarr;|] |> move (-halfWidth-280-p.x,-200)
        , toForm [markdown|jump &uarr;|]        |> move (-halfWidth-80-p.x,-100)
        , toForm [markdown|run with SHIFT|]     |> move (360-p.x,50)
        , toForm (if s == Won  then [markdown|#You won!|] else spacer 1 1)   |> move (0, 100)
        , toForm (if s == Lost then [markdown|#You died!|] else spacer 1 1 ) |> move (0, 0)
        --, toForm <| asText p 
        ]
  
display : (Int, Int) -> Game -> Element
display (w, h) g = 
    let activeP    = getActive g.characters 
        characters = group  <| map make <| g.characters
        obstacles  = group  <| map (makeObstacle activeP) g.obstacles
        helpText   = administrivia activeP g.state
    in container w h middle <| collage mainWidth mainHeight  <|
         [obstacles, helpText, characters]
      
main = lift2 display Window.dimensions gameState
