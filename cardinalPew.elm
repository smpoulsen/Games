import Char
import Keyboard
import Text
import Time
import Window

listToMaybe : [a] -> Maybe a
listToMaybe x = if | x == []   -> Nothing
                   | otherwise -> Just (head x)


------ INPUTS ------
type Input = { yDir:Int, fire:Bool, shift:Bool, swap:Bool, delta:Time }

delta = lift (\t -> t/10) (fps 60)

input = sampleOn delta (Input <~ lift .y Keyboard.arrows
                               ~ Keyboard.space
                               ~ Keyboard.shift
                               ~ Keyboard.isDown (Char.toCode 'C')
                               ~ delta)

------ MODEL -------
data State        = Alive | Dead | Playing
data WeaponTypes  = Blaster | WaveBeam | Bomb
type Game p e w   = { player:(Player p w), enemies:[(Enemy e)], shots:[Projectiles], state:State }
type Player p w   = { p | x:Float, y:Float, vy:Float, firing:Bool, weapons:[(Weapon w)], boost:Bool, health:Int }
type Enemy  e     = { e | x:Float, y:Float, vy:Float, sides:Int, radius:Float, alive:Bool  }
type Weapon w     = { w | kind:WeaponTypes, active:Bool, cooldown:Time }
type Projectiles  = { kind:WeaponTypes, x:Float, y:Float, vx:Float, vy:Float, alive:Bool, radius:Float }
type GameObject o = { o | vx:Float} 

--defaultGame : Game
defaultGame = { player=player1, enemies=[enemy1, enemy2], shots=[], state=Playing }

--player1 : Player (GameObject {})
player1 = { x=-halfWidth+padding, y=0, vx=0, vy=0, firing=False, weapons=[weapon1, weapon2], health=100, boost=False }
weapon1 = { kind=Blaster, active=True,   cooldown=(second), x=0, y=0, vx=0, alive=False }
weapon2 = { kind=WaveBeam, active=False, cooldown=(second), x=0, y=0, vx=0, alive=False }
enemy1 =  { x=halfWidth+100, y=0, vx=-1, vy=0, sides=5, radius=30, alive=True }
enemy2 =  { x=halfWidth, y=100, vx=-1, vy=0, sides=7, radius=15, alive=True }

------ UPDATE ------
collision o1 o2 = (o1.x >= o2.x - o2.radius && o1.x <= o2.x + o2.radius) &&
                  (o1.y >= o2.y - o2.radius && o1.y <= o2.y + o2.radius)

moveP yDir boost p = if p.health > 0 
                     then { p | vy <- if | boost     -> toFloat <| 4 * yDir
                                         | otherwise -> toFloat <| 2 * yDir }
                     else p

--Objects need to constantly move <-; Relies on time.
moveO t n o = { o | vx <- n*t/10   }

physics t p = { p | y <- p.y + t * p.vy * 2 |> clamp (-halfHeight) (halfHeight)
                  , x <- p.x + t * p.vx * 10 }

healthLost es p = { p | health <- p.health - (length es) }

outOfBounds o = o.x > halfWidth || o.x < -halfWidth

--WEAPONS
activeWeapon : [(Weapon w)] -> Weapon w
activeWeapon ws = ws |> listToMaybe . filter (\x -> x.active) 
                     |> maybe (head ws) (\x -> x) 

swapWeapons i p = { p | weapons <- if | i.swap -> map (\x -> { x | active <- not x.active }) p.weapons
                                      | otherwise -> p.weapons }
       
shoot i s p = 
    let w = activeWeapon p.weapons
    in if i.fire 
       then { kind=w.kind, x=p.x+17, y=p.y, vy=0,
              vx=if | w.kind == Blaster  -> 3.0
                    | w.kind == WaveBeam -> 5.0 
            , radius= if | w.kind == Blaster  -> 4.0
                         | w.kind == WaveBeam -> 10.0 
            , alive=True } :: s
       else s 

shotsHit s e = if any (\shot -> collision shot e) s then { e | alive <-  False }
               else e

--STEPPERS
stepPlayer i g = 
    let gotPast = filter (\e -> e.x+e.radius <= g.player.x) g.enemies
    in g.player |> healthLost gotPast . physics i.delta . moveP i.yDir i.shift . swapWeapons i

stepEnemies i g = 
    let inPlay = filter (\e -> e.x >= -halfWidth && e.alive) g.enemies
    in map (\x -> physics i.delta . moveO i.delta (-1) . shotsHit g.shots <| x) inPlay

stepWeapons i g = 
    let cleanShots = filter (\o -> o.x <= halfWidth-padding ) g.shots
    in  g.player |> shoot i cleanShots
                 |> map (\x -> physics i.delta <| moveO i.delta 1 <| x)

stepGame i g = { g | player <- stepPlayer i g
                   , enemies <- stepEnemies i g
                   , shots <- stepWeapons i g
               }

gameState = foldp stepGame defaultGame input

------ DISPLAY ------
(gameWidth, gameHeight) = (1000, 600)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
padding = 50

make p = ngon 3 20 |> outlined (solid white)
                   |> move (p.x, p.y)

makeShots s = 
    if | s.kind == Blaster  -> circle 4   |> filled red
                                          |> move (s.x, s.y)
       | s.kind == WaveBeam -> rect 15 10 |> filled purple
                                          |> move (s.x, s.y)
       | otherwise -> rect 5 20 |> filled lightBlue |> move (s.x, s.y)

makeEnemies e = if e.alive then 
    ngon e.sides e.radius |> outlined (solid white)
                          |> move (e.x, e.y)
    else spacer 1 1 |> toForm

txt = leftAligned . Text.color white . toText 

display : (Int, Int) -> Game p e w -> Element
display (w, h) g = 
    container w h middle <| collage gameWidth gameHeight  <|
         concat [
            [ rect gameWidth gameHeight |> filled black
            , "Health: "++ show g.player.health ++ "%" |> txt |> toForm |> move (-halfWidth+padding, halfHeight-padding/2)
            , make g.player
            ], map makeShots g.shots, map makeEnemies g.enemies] 

main = lift2 display Window.dimensions gameState
