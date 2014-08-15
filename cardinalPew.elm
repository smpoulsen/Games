import Char
import Keyboard
import String
import Text
import Time
import Window
import Generator
import Generator.Standard

import Tpoulsen.Lib (listToMaybe)

------ INPUTS ------
type Input = { yDir:Int, fire:(Time, Bool), bomb:(Time, Bool), shift:Bool, swap:(Time, Bool)
             , restart:Bool, pause:(Time, Bool), sinceStart:Time, windowSize:(Int, Int), delta:Time }

delta : Signal Time
delta = lift (\t -> t/9) (fps 60)

input : Signal Input
input = sampleOn delta (Input <~ lift .y Keyboard.arrows
                               ~ Time.timestamp Keyboard.space
                               ~ Time.timestamp (Keyboard.isDown (Char.toCode 'B'))
                               ~ Keyboard.shift
                               ~ Time.timestamp (Keyboard.isDown (Char.toCode 'C'))
                               ~ Keyboard.isDown (Char.toCode 'R')
                               ~ Time.timestamp (Keyboard.isDown (Char.toCode 'P'))
                               ~ every (second)
                               ~ Window.dimensions
                               ~ delta )


------ MODEL -------
--Aliases for type definition convenience.
type PlayerO  = Player (GameObject {})
type EnemyO   = Enemy  (GameObject {})
type ShotO    = Shot   (GameObject {})
type BombO    = ShotO
type GameOs   = Game   (GameObject {}) (GameObject {}) (GameObject {})
type StdGen   = Generator.Generator Generator.Standard.Standard
type Particle = GameObject {}

--Model types.
data PlayState    = Playing | Paused
data WeaponTypes  = Blaster | WaveBeam | Bomb
type Game p e s   = { player:(Player p), enemies:[Enemy e], shots:[Shot s]
                    , bombs:[BombO], particles:[Particle], score:Int, totalShots:Int
                    , paused:Time, playState:PlayState, rGen:StdGen, halfWidth:Float, halfHeight:Float }
type Player p     = { p | weapons:[Weapon], bombs:Int, boost:Bool, health:Int }
type Enemy  e     = { e | sides:Int, created:Time  }
type Weapon       = { kind:WeaponTypes, active:Bool, cooldown:Time, lastSwapped:Time, fillColor:Color }
type Shot   s     = { s | kind:WeaponTypes, fired:Time }
type GameObject o = { o | x:Float, y:Float, vy:Float, vx:Float, radius:Float, alive:Bool} 

defaultGame : GameOs
defaultGame = { player=player1, enemies=[enemy1], shots=[], bombs=[], particles=[]
              , score=0, totalShots=0, paused=0, playState=Paused, rGen=gen, halfWidth=800, halfHeight=300 }

player1  : PlayerO
player1  = { x=-500+padding, y=0, vx=0, vy=0, weapons=[blaster, waveBeam]
           , bombs=3, radius=15, health=100, boost=False, alive=True }
blaster  : Weapon
blaster  = { kind=Blaster, active=True,   cooldown=(50 * millisecond), lastSwapped=0, fillColor=lightRed }
waveBeam : Weapon
waveBeam = { kind=WaveBeam, active=False, cooldown=(second), lastSwapped=0, fillColor=lightPurple }
enemy1   : EnemyO
enemy1   =  { x=1000, y=0, vx=-1, vy=0, sides=5, radius=30, alive=True, created=0.0 }


------ UPDATE ------

--OBJECT GENERATORS
randomSeed = 1
gen : StdGen
gen = Generator.Standard.generator randomSeed

genEnemies : Time -> Float -> Float -> StdGen -> (EnemyO, StdGen)
genEnemies t halfW halfH g = 
  let ([y',vx', s',r'], g') = Generator.listOf (Generator.floatRange (-1, 1)) 4 g
      eY  = y' * halfH |> clamp (-halfH+padding/2) (halfH-hud-eR)
      eVx = (abs vx')*(-4)  |> clamp -2 -3  
      eS  = (abs s') * 10   |> round |> clamp 3 10 
      eR  = (abs r') * 30   |> clamp 10 30
  in ({ x=halfW+100, y=eY, vx=eVx, vy=0, sides=eS, radius=eR, alive=True, created=t }, g')

genParticles : StdGen -> EnemyO -> (StdGen, Particle)
genParticles g o = 
    let ([vx',vy'], g') = Generator.listOf (Generator.floatRange (-1, 1)) 2 g
        pVx = abs vx' * 20 
        pVy = vy' * 10
    in  (g',{ x=o.x, y=o.y, vx=pVx,  vy=pVy,  radius=o.radius, alive=True })

nParticles : Int -> EnemyO -> StdGen -> [Particle]
nParticles i e gen = 
    let (newG, newP) = genParticles gen e
    in case i of
            0 -> []
            otherwise -> newP :: (nParticles (i-1) e newG)

--COLLISION & BOUNDS CHECKS
collision : GameObject o -> GameObject o -> Bool
collision o1 o2 = (o1.x + o1.radius >= o2.x - o2.radius && 
                   o1.x - o1.radius <= o2.x + o2.radius) &&
                  (o1.y + o1.radius >= o2.y - o2.radius && 
                   o1.y - o1.radius <= o2.y + o2.radius)

outOfBounds : Float -> Float -> GameObject o -> Bool
outOfBounds halfW halfH o = 
    o.x > halfW+100 || o.x < -halfW || 
    o.y > halfH+100 || o.y < -halfH-100 ||
    (not o.alive)

--PHYSICS
genericPhysics : Time -> Float -> GameObject o -> GameObject o
genericPhysics t halfH p = { p | y <- p.y + t * p.vy  |> clamp (-halfH+padding/2) (halfH-hud)
                               , x <- p.x + t * p.vx  }

shotPhysics : Time -> Float -> ShotO -> ShotO 
shotPhysics t halfH s  = { s | y <- if | s.kind /= WaveBeam -> s.y  |> clamp (-halfH) (halfH-hud)
                                       | otherwise -> s.y - 2*(sin <| (s.x)/50 ) |> clamp (-halfH) (halfH)
                             , x <- if | s.kind /= WaveBeam -> s.x + t * s.vx 
                                       | otherwise  -> s.x + t * s.vx  }
    
particlePhysics : Time -> Float -> GameObject o -> GameObject o
particlePhysics t halfH p = { p | y <- p.y + t * p.vy * 2 |> clamp (-halfH-100) (halfH+100)
                                , x <- p.x + t * p.vx }

bombPhysics : Time -> PlayerO -> BombO -> BombO
bombPhysics t p b = { b | x <- b.x + t * b.vx * 50 |> clamp p.x 0 }

--MOVEMENT
moveP : Input -> PlayerO -> PlayerO
moveP i p = 
    if p.health > 0 
    then { p | vy <- if | i.shift   -> toFloat <| 6 * i.yDir
                        | otherwise -> toFloat <| 2 * i.yDir 
             , x  <- -(toFloat (fst i.windowSize)/2)+padding
         }
    else p

--HEALTH & SCORE
healthLost : [EnemyO] -> PlayerO -> PlayerO
healthLost es p = 
    let gotPast = filter (\e -> e.x+e.radius <= p.x) es
    in { p | health <- if | any (\e -> collision p e) es -> 0
                          | otherwise -> p.health - (sum . map (round . (\x-> x.radius/10 )) <| gotPast) }

isAlive : PlayerO -> PlayerO
isAlive p = { p | alive <- if | p.health <= 0 -> False
                              | otherwise     -> True
            } 

scoreMod score es = 
    let dead = length <| filter (\e -> not e.alive) es
    in score + dead

--WEAPONS
activeWeapon : [Weapon] -> Weapon
activeWeapon ws = ws |> listToMaybe . filter (\x -> x.active) 
                     |> maybe (head ws) id

swapWeapons : Input -> PlayerO -> PlayerO
swapWeapons i p = 
    let sinceSwapped = (fst i.swap) - (activeWeapon p.weapons).lastSwapped
        toggleWeapon =  map (\x -> { x | active      <- not x.active
                                       , lastSwapped <- (fst i.swap) })
        okayToSwap   = (snd i.swap) && sinceSwapped > 5
    in { p | weapons <- if | okayToSwap -> toggleWeapon p.weapons
                           | otherwise  -> p.weapons }

shotCanFire : Input -> PlayerO -> [ShotO] -> Bool
shotCanFire i p s =
    let w = activeWeapon p.weapons
        f = s |> listToMaybe |> maybe i.delta (\x -> x.fired)
    in p.health > 0 && (snd i.fire) && (fst i.fire) - f >= w.cooldown 

newShot : Input -> PlayerO -> [ShotO] -> [ShotO]
newShot i p s = 
    let w = activeWeapon p.weapons
    in if shotCanFire i p s
       then 
            if w.kind == Blaster 
            then { kind=w.kind, x=p.x+17, y=p.y,    vy=0, vx=5.0, radius=4.0, alive=True, fired=(fst i.fire) } :: s
            else { kind=w.kind, x=p.x+17, y=p.y+15, vy=0, vx=8.0, radius=6.0, alive=True, fired=(fst i.fire) } ::
                 { kind=w.kind, x=p.x+17, y=p.y,    vy=0, vx=8.0, radius=6.0, alive=True, fired=(fst i.fire) } ::
                 { kind=w.kind, x=p.x+17, y=p.y-15, vy=0, vx=8.0, radius=6.0, alive=True, fired=(fst i.fire) } :: s
       else s 

--bombCanFire : Input -> PlayerO -> [BombO] -> Bool
bombCanFire i p b = 
    let lastB = b |> listToMaybe |> maybe 0 (\b -> b.fired)
    in p.health > 0 && p.bombs > 0 && (snd i.bomb) && (fst i.bomb) - lastB > 5

--fireBomb : Input -> PlayerO -> [BombO] -> [BombO]
fireBomb i p bs = 
    if p.bombs > 0 && bombCanFire i p bs
    then { x=p.x, y=p.y, vx=1, vy=0, radius=10, alive=True, kind=Bomb, fired=(fst i.bomb) } :: bs
    else filter (\b -> b.alive ) bs

--shotBomb : Input -> [BombO] -> PlayerO -> PlayerO
shotBomb i bs p = { p | bombs <- if | bombCanFire i p bs  -> p.bombs - 1
                                    | otherwise -> p.bombs 
                  }

--explodeBomb : Time -> BombO -> BombO
explodeBomb t b = 
    if (inSeconds t) - (inSeconds b.fired) < 0.5
    then { b | radius <- b.radius + 4 } 
    else { b | alive <- False }

shotsHit : [ShotO] -> EnemyO -> EnemyO
shotsHit s e = if any (\shot -> collision shot e) s then { e | alive <-  False }
               else e

addShots i g = 
    if shotCanFire i g.player g.shots
    then g.totalShots + 1
    else g.totalShots

--STEPPERS
--stepPlayer : Input -> Game PlayerO EnemyO ShotO -> Game PlayerO EnemyO ShotO
stepPlayer i g = g.player |> isAlive . shotBomb i g.bombs . healthLost g.enemies . 
                             genericPhysics i.delta g.halfHeight . moveP i . swapWeapons i

stepEnemies i g  = 
    let inPlay = filter (\e -> not . outOfBounds g.halfWidth g.halfHeight <| e) g.enemies
        lastC  = inPlay |> listToMaybe |> maybe (enemy1) id
        es'    = if (i.sinceStart - lastC.created >= 0.5) 
                 then (fst <| genEnemies i.sinceStart g.halfWidth g.halfHeight g.rGen) :: inPlay 
                 else inPlay
    in map (\x -> genericPhysics i.delta g.halfHeight . shotsHit g.bombs . shotsHit g.shots <| x) es'

stepWeapons i g = 
    let cleanShots = g.shots |> filter (\o -> not . outOfBounds g.halfWidth g.halfHeight <| o) 
                             |> filter (\s -> if s.kind == Blaster 
                                              then not <| any (\e -> collision s e) g.enemies
                                              else s.alive)
    in  cleanShots |> newShot i g.player
                   |> map (\x -> shotPhysics i.delta g.halfHeight <| x)

stepBombs i g = 
    let bs'   = fireBomb i g.player g.bombs |> filter (\b -> b.alive)
        lastB = bs' |> listToMaybe |> maybe 0 (\b -> b.fired)
    in bs' |> map (\b -> bombPhysics i.delta g.player <| explodeBomb i.sinceStart <| b)

stepParticles i g =
    let currentPs    = filter (\p -> not . outOfBounds g.halfWidth g.halfHeight <| p) g.particles
        deadEs       = filter (\e -> not e.alive) g.enemies
        allParticles = concatMap (\e -> nParticles e.sides e g.rGen) deadEs
    in allParticles ++ currentPs |> map (\x -> particlePhysics i.delta g.halfHeight <| x)

stepPlayState i g =
    if (snd i.pause) && (fst i.pause) - g.paused > 5
    then  if | g.playState == Paused -> Playing
             | otherwise -> Paused 
    else g.playState

stepGame i g = 
    let paused = g.playState == Paused
        playState' = stepPlayState i g
    in if i.restart && (paused || g.player.health <= 0)
       then defaultGame
       else
        { g | player     <- if not paused then stepPlayer    i g else g.player
            , enemies    <- if not paused then stepEnemies   i g else g.enemies
            , shots      <- if not paused then stepWeapons   i g else g.shots
            , bombs      <- if not paused then stepBombs     i g else g.bombs
            , particles  <- if not paused then stepParticles i g else g.particles
            , rGen       <- if not paused then snd <| genEnemies i.sinceStart g.halfWidth g.halfHeight g.rGen 
                            else g.rGen
            , score      <- if not paused && g.player.alive then scoreMod g.score g.enemies 
                            else g.score
            , totalShots <- if not paused then addShots i g else g.totalShots
            , playState  <- playState'
            , paused     <- if g.playState /= playState' then (fst i.pause) else g.paused
            , halfWidth  <- toFloat (fst i.windowSize)/2
            , halfHeight <- toFloat (snd i.windowSize)/2
        }

gameState : Signal GameOs
gameState = foldp stepGame defaultGame input

------ DISPLAY ------
padding = 50
hud     = 50

txt : Color -> String -> Element
txt c = centered . monospace . Text.height 16 . Text.color c . toText 

--makeHud : GameOs -> Input -> Form
makeHud g i = 
    let p        = g.player
        equipped = activeWeapon p.weapons
        lastShot = g.shots |> listToMaybe |> maybe 0 (\x -> x.fired)
        paused   = g.playState == Paused
        score    = String.padLeft 5 '0' . show <| g.score
    in  group 
            [
              "Health: " ++ show p.health ++ "%" |> txt white 
                                                 |> toForm 
                                                 |> move (-g.halfWidth+padding*2, g.halfHeight-padding/2)
            , "Weapon: " ++ show equipped.kind   |> txt equipped.fillColor 
                                                 |> toForm 
                                                 |> move (-g.halfWidth/2.5, g.halfHeight-padding/2)
            , "Bombs: "  ++ ("O" |> repeat p.bombs |> String.join " ")
                                                   |> txt lightBlue 
                                                   |> toForm 
                                                   |> move (0, g.halfHeight-padding/2)
            , "Score: " ++ score |> txt white 
                                 |> toForm 
                                 |> move (g.halfWidth-padding*2, g.halfHeight-padding/2)
            , if paused 
              then group
              [ "PAUSED"                 |> txt lightRed |> toForm |> move (0, padding*2.5)
              , "'p' - to resume"          |> txt white |> toForm |> move (0, padding*2)
              , "'&uarr;&darr;' - to move ship" |> txt white |> toForm |> move (-padding*4, padding)
              , "'shift' - to boost speed" |> txt white |> toForm |> move (-padding*4, 0)
              , "'space' - to shoot weapon"|> txt white |> toForm |> move (padding*4, padding)
              , "'c' - to change weapons"  |> txt white |> toForm |> move (padding*4, 0)
              , "'b' - to deploy a bomb"    |> txt white |> toForm |> move (0, -padding)
              , "'r' - to restart game (from paused)"|> txt white |> toForm |> move (0, -padding*2)
              ]
              else spacer 0 0 |> toForm           
            ]

makeDeathScreen g =
    let finalScore = g.score      |> String.padLeft 5 '0' . show
        totalShots = g.totalShots |> String.padLeft 5 '0' . show 
        accuracy   = 100.0 * (toFloat g.score)/(toFloat g.totalShots) |> String.left 5 . show
    in group [
               "YOU DIED" |> txt lightRed |> toForm |> move (0, padding*2)
             , "Final Score:  " ++ finalScore |> txt white |> toForm |> move (0, padding)
             , "Total Shots:  " ++ totalShots |> txt white |> toForm |> move (0, 0)
             , "Accuracy:     " ++  accuracy  ++ "%" |> txt white |> toForm |> move (0, -padding)
             , "Press 'r' to play again!" |> txt white |> toForm |> move (0, -padding*2)
             ]

make : PlayerO -> Form
make p = 
    if p.health > 0 then
        ngon 3 20   |> outlined (solid white)
                    |> move (p.x, p.y)
    else spacer 0 0 |> toForm

makeShots : ShotO -> Form
makeShots s = 
    let fill = if | s.kind == Blaster -> lightRed
                  | s.kind == WaveBeam -> lightPurple
                  | otherwise -> lightBlue
    in circle s.radius |> outlined (solid fill)
                       |> move (s.x, s.y)

makeBombs : BombO -> Form
makeBombs b =
    circle b.radius |> outlined (solid lightBlue)
                    |> move (b.x, b.y)

makeEnemies : EnemyO -> Form
makeEnemies enemy = if enemy.alive then 
    ngon enemy.sides enemy.radius |> outlined (solid white)
                                  |> move (enemy.x, enemy.y)
    else spacer 1 1 |> toForm

makeParticles : Particle -> Form
makeParticles p = 
    let particleGradient = 
        radial (0,0) p.radius (7,-15) 50
          [ (   0, yellow)
          , (0.75, lightRed)
          , (   1, white)
          ]
    in rect p.radius 2 |> gradient particleGradient
                       |> move (p.x, p.y)
                       |> rotate (tan (p.y+p.vy))


display : (Int, Int) -> GameOs -> Input -> Element
display (w', h') g i = 
    let (w, h) = (toFloat w', toFloat h')
        (halfW, halfH) = (w/2, h/2)
        equipped = activeWeapon g.player.weapons
    in container w' h' middle <| collage w' h'  <|
           concat [
              [ rect w h |> filled black
              , if g.player.health > 0 then makeHud g i else makeDeathScreen g
              , make g.player
              ], map makeShots g.shots
               , map makeEnemies g.enemies
               , map makeBombs g.bombs
               , map makeParticles g.particles] 

main = lift3 display Window.dimensions gameState input
