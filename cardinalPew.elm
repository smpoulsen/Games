import Char
import Keyboard
import Text
import Time
import Window
import Generator
import Generator.Standard

import Tpoulsen.Lib (listToMaybe)

------ INPUTS ------
type Input = { yDir:Int, fire:(Time, Bool), shift:Bool, swap:Bool, pause:Bool, sinceStart:Time, delta:Time }

delta = lift (\t -> t/8) (fps 60)

input = sampleOn delta (Input <~ lift .y Keyboard.arrows
                               ~ Time.timestamp Keyboard.space
                               ~ Keyboard.shift
                               ~ Keyboard.isDown (Char.toCode 'C')
                               ~ Keyboard.isDown (Char.toCode 'P')
                               ~ every second
                               ~ delta)

--startTime = 

------ MODEL -------

data WeaponTypes  = Blaster | WaveBeam | Bomb
type Game p e w   = { player:(Player p w), enemies:[(Enemy e)], shots:[Projectiles], paused:Bool, rGen:Generator.Generator Generator.Standard.Standard }
type Player p w   = { p | x:Float, y:Float, vy:Float, weapons:[(Weapon w)], radius:Int, boost:Bool, score:Int, health:Int }
type Enemy  e     = { e | x:Float, y:Float, vy:Float, sides:Int, radius:Float, alive:Bool, created:Time  }
type Weapon w     = { w | kind:WeaponTypes, active:Bool, heat:Int, cooldown:Time, fillColor:Color }
type Projectiles  = { kind:WeaponTypes, x:Float, y:Float, vx:Float, vy:Float, alive:Bool, radius:Float, fired:Time }
type GameObject o = { o | vx:Float} 

--defaultGame : Game
defaultGame = { player=player1, enemies=[enemy1], shots=[], paused=True, rGen=gen }

--player1 : Player (GameObject {})
player1 = { x=-halfWidth+padding, y=0, vx=0, vy=0, weapons=[weapon1, weapon2], radius=20, score=0, health=100, boost=False }
weapon1 = { kind=Blaster, active=True,   heat=0, cooldown=(50 * millisecond), fillColor=red }
weapon2 = { kind=WaveBeam, active=False, heat=0, cooldown=(second), fillColor=purple }
enemy1 =  { x=halfWidth+100, y=0, vx=-1, vy=0, sides=5, radius=30, alive=True, created=0.0 }

--ENEMY GENERATOR
randomSeed = 1
gen = Generator.Standard.generator randomSeed

genEnemies t g = let ([y',s',r'], g') = Generator.listOf (Generator.floatRange (-1, 1)) 3 g
                     eY = clamp (-halfHeight+eR) (halfHeight-hud-eR) <| y' * halfHeight
                     eS = clamp 3 10 <| round <| (abs s') * 10
                     eR = clamp 10 30 <| (abs r') * 30 
                in ({ x=halfWidth+100, y=eY, vx=-1, vy=0, sides=eS, radius=eR, alive=True, created=t }, g')

------ UPDATE ------
collision o1 o2 = (o1.x >= o2.x - o2.radius && o1.x <= o2.x + o2.radius) &&
                  (o1.y + o1.radius >= o2.y - o2.radius && o1.y - o1.radius <= o2.y + o2.radius)

moveP yDir boost p = if p.health > 0 
                     then { p | vy <- if | boost     -> toFloat <| 4 * yDir
                                         | otherwise -> toFloat <| 2 * yDir }
                     else p

--Objects need to constantly move <-; Relies on time.
moveO t n o = { o | vx <- n*t/10     }

physics t p = { p | y <- p.y + t * p.vy * 2 |> clamp (-halfHeight) (halfHeight-hud)
                  , x <- p.x + t * p.vx * 10 }

shotPhysics t s  = { s | y <- if | s.kind /= WaveBeam -> s.y  |> clamp (-halfHeight) (halfHeight-hud)
                                 | otherwise -> s.y - 2*(sin <| (s.x)/50 ) |> clamp (-halfHeight) (halfHeight-hud)
                       , x <- if | s.kind /= WaveBeam -> s.x + t * s.vx * 15
                                 | otherwise  -> s.x + t * s.vx * 25 }

healthLost es p = 
    let gotPast = filter (\e -> e.x+e.radius <= p.x) es
    in { p | health <- if | any (\e -> collision p e) es -> 0
                          | otherwise -> p.health - (sum . map (round . (\x-> x.radius/10 )) <| gotPast) }

--scoreMod p s es = 

outOfBounds o = o.x > halfWidth || o.x < -halfWidth

--WEAPONS
activeWeapon : [(Weapon w)] -> Weapon w
activeWeapon ws = ws |> listToMaybe . filter (\x -> x.active) 
                     |> maybe (head ws) (\x -> x) 

swapWeapons i p = { p | weapons <- if | i.swap -> map (\x -> { x | active <- not x.active }) p.weapons
                                      | otherwise -> p.weapons }

shoot i p s = 
    let w = activeWeapon p.weapons
        f = s |> listToMaybe |> maybe i.delta (\x -> x.fired)
    in if p.health > 0 && (snd i.fire) && (fst i.fire) - f >= w.cooldown 
       then 
            if w.kind == Blaster 
            then { kind=w.kind, x=p.x+17, y=p.y, vy=0,    vx=3.0,  radius=4.0, alive=True, fired=(fst i.fire) } :: s
            else { kind=w.kind, x=p.x+17, y=p.y+15, vy=0, vx=20.0, radius=6.0, alive=True, fired=(fst i.fire) } ::
                 { kind=w.kind, x=p.x+17, y=p.y, vy=0,    vx=20.0, radius=6.0, alive=True, fired=(fst i.fire) } ::
                 { kind=w.kind, x=p.x+17, y=p.y-15, vy=0, vx=20.0, radius=6.0, alive=True, fired=(fst i.fire) } :: s
       else s 

shotsHit s e = if any (\shot -> collision shot e) s then { e | alive <-  False }
               else e


--STEPPERS
stepPlayer i g = g.player |> healthLost g.enemies . physics i.delta . moveP i.yDir i.shift . swapWeapons i


stepEnemies i g  = 
    let inPlay = filter (\e -> e.x >= -halfWidth && e.alive) (g.enemies)
        lastC  = head inPlay
        dead   = filter (\e -> not e.alive) g.enemies
        es'    = if (i.sinceStart - lastC.created >= 0.5) 
                 then (fst <| genEnemies i.sinceStart g.rGen) :: inPlay 
                 else inPlay
    in map (\x -> physics i.delta . moveO i.delta (-1) . shotsHit g.shots <| x) es'

stepWeapons i g = 
    let cleanShots = g.shots |> filter (\o -> o.x <= halfWidth) 
                             |> filter (\s -> if s.kind == Blaster 
                                              then not <| any (\e -> collision s e) g.enemies
                                              else s.alive)
    in  cleanShots |> shoot i g.player
                   |> map (\x -> shotPhysics i.delta  . moveO i.delta 1 <| x)

stepGame i g = 
    { g | player <- if not g.paused then stepPlayer i g else g.player
        , enemies <- if not g.paused then stepEnemies i g else g.enemies
        , shots <- if not g.paused then stepWeapons i g else g.shots
        , rGen <- if not g.paused then snd <| genEnemies i.sinceStart g.rGen else g.rGen
        , paused <- if | i.pause -> not g.paused
                       | otherwise -> g.paused
    }

gameState = foldp stepGame defaultGame input

------ DISPLAY ------
(gameWidth, gameHeight) = (1000, 600)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)
padding = 50
hud     = 50

makeHud p s paused i = 
    let equipped = activeWeapon p.weapons
        lastShot = s |> listToMaybe |> maybe 0 (\x -> x.fired)
    in if p.health > 0  
       then group 
            [
              "Health: " ++ show p.health ++ "%" |> txt white |> toForm |> move (-halfWidth+padding*2, halfHeight-padding/2)
            , "Weapon: " ++ show equipped.kind |> txt equipped.fillColor |> toForm |> move (-halfWidth/2, halfHeight-padding/2)
            --, "Cooldown: " ++ show (max 0 <| equipped.cooldown + lastShot - i.sinceStart) |> txt white |> toForm
            , "pewPew!" |> txt darkGreen |> toForm |> move (0, halfHeight-padding/2)
            --, "Score: " ++ show p.score |> txt white |> toForm |> move (0, halfHeight-padding/2)
            --, show (i.sinceStart) |> txt white |> toForm |> move (halfWidth-padding, halfHeight-padding/2)
            , if paused 
              then "PAUSED\n'p' to resume\n&uarr;&darr; to move\n'space' to shoot\n'shift' for boost\n'c' to change weapons" 
                              |> txt white |> toForm 
              else spacer 0 0 |> toForm           
            ]
        else spacer 1 1 |> toForm


make p = 
    if p.health > 0 then
        ngon 3 20 |> outlined (solid white)
                  |> move (p.x, p.y)
    else "YOU DIED" |> txt white |> toForm

makeShots s = 
    let fill = if | s.kind == Blaster -> red
                  | s.kind == WaveBeam -> purple
                  | otherwise -> lightBlue
    in circle s.radius |> outlined (solid fill)
                    |> move (s.x, s.y)


makeEnemies e = if e.alive then 
    ngon e.sides e.radius |> outlined (solid white)
                          |> move (e.x, e.y)
    else spacer 1 1 |> toForm

txt c = centered . monospace . Text.color c . toText 

display : (Int, Int) -> Game p e w -> Input -> Element
display (w, h) g i = 
    let equipped = activeWeapon g.player.weapons
    in container w h middle <| collage gameWidth gameHeight  <|
           concat [
              [ rect gameWidth gameHeight |> filled black
              , makeHud g.player g.shots g.paused i 
              , make g.player
              ], map makeShots g.shots, map makeEnemies g.enemies] 

main = lift3 display Window.dimensions gameState input
