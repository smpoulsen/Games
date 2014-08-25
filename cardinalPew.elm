{-
Travis Poulsen - 2014
-}

import Char
import Keyboard
import String
import Text
import Time
import Window

import Generator
import Generator.Standard

import Tpoulsen.Lib (listToMaybe)
import Tpoulsen.Vectors (collision, findVerticies)

------ INPUTS ------
type Input = { yDir:Int, fire:(Time, Bool), bomb:(Time, Bool), shift:Bool, swap:(Time, Bool)
             , restart:Bool, pause:(Time, Bool), sinceStart:Time
             , windowSize:(Int, Int), delta:Time }

timeMod : number
timeMod = 12
delta : Signal Time
delta = lift (\t -> t/timeMod) (fps 60)

input : Signal Input
input = sampleOn delta (Input <~ lift .y Keyboard.arrows
                               ~ Time.timestamp Keyboard.space
                               ~ Time.timestamp (Keyboard.isDown (Char.toCode 'B'))
                               ~ Keyboard.shift
                               ~ Time.timestamp (Keyboard.isDown (Char.toCode 'C'))
                               ~ Keyboard.isDown (Char.toCode 'R')
                               ~ Time.timestamp (Keyboard.isDown (Char.toCode 'P'))
                               ~ every (500 * millisecond)
                               ~ Window.dimensions
                               ~ delta )

------ MODEL -------

--Aliases for type definition convenience.
type PlayerO  = Player (GameObject {})
type EnemyO   = Enemy  (GameObject {})
type ShotO    = Shot   (GameObject {})
type BombO    = ShotO
type Particle = ShotO
type GameOs   = Game   (GameObject {}) (GameObject {}) (GameObject {})
type StdGen   = Generator.Generator Generator.Standard.Standard

--Model types.
data PlayState    = Start | Controls | Playing | Paused | GameOver
data WeaponTypes  = Blaster | WaveBeam | Bomb | Debris
type Game p e s   = { player:(Player p), enemies:[Enemy e], shots:[Shot s]
                    , bombs:[BombO], particles:[Particle], score:Int, totalShots:Int
                    , paused:Time, playTime:Time, playState:PlayState, rGen:StdGen
                    , halfWidth:Float, halfHeight:Float }
type Player p     = { p | weapons:[Weapon], bombs:Int, boost:Bool, health:Int }
type Enemy  e     = { e | created:Time  }
type Shot   s     = { s | kind:WeaponTypes, fired:Time }
type GameObject o = { o | x:Float, y:Float, vy:Float, vx:Float, radius:Float, sides:Int, alive:Bool } 
type Weapon       = { kind:WeaponTypes, active:Bool, cooldown:Time, lastSwapped:Time, fillColor:Color }

--Predefined objects.
defaultGame : GameOs
defaultGame = { player=player1, enemies=[enemy1], shots=[], bombs=[], particles=[]
              , score=0, totalShots=0, paused=0, playTime=0, playState=Start, rGen=gen
              , halfWidth=800, halfHeight=300 }

player1  : PlayerO
player1  = { x=-500+padding, y=0, vx=0, vy=0, weapons=[blaster, waveBeam]
           , bombs=3, radius=20, sides=3, health=100, boost=False, alive=True }
blaster  : Weapon
blaster  = { kind=Blaster, active=True,   cooldown=(50*millisecond), lastSwapped=0, fillColor=lightRed }
waveBeam : Weapon
waveBeam = { kind=WaveBeam, active=False, cooldown=(second), lastSwapped=0, fillColor=lightPurple }
bomb     : Weapon
bomb     = { kind=Bomb, active=False, cooldown=(5), lastSwapped=0, fillColor=lightBlue }
enemy1   : EnemyO
enemy1   = { x=1000, y=0, vx=-1, vy=0, sides=5, radius=30, alive=True, created=0.0 }

------ UPDATE ------

--OBJECT GENERATORS
randomSeed = 1
gen : StdGen
gen = Generator.Standard.generator randomSeed

genEnemies : Time -> Float -> Float -> StdGen -> (EnemyO, StdGen)
genEnemies t halfW halfH g = 
  let ([y',vx', s',r'], g') = Generator.listOf (Generator.floatRange (-1, 1)) 4 g
      eY  = y' * halfH |> clamp (-halfH+padding/2) (halfH-hud-eR)
      eVx = (abs vx')*(-3)  |> clamp -2 -3  
      eS  = (abs s') * 10   |> round |> clamp 4 10 
      eR  = (abs r') * 30   |> clamp 10 30
  in ({ x=halfW+100, y=eY, vx=eVx, vy=0, sides=eS, radius=eR, alive=True, created=t }, g')

genParticles : Time -> StdGen -> GameObject o -> (StdGen, Particle)
genParticles t g o = 
    let ([vx',vy'], g') = Generator.listOf (Generator.floatRange (-1, 1)) 2 g
        pVx = abs vx' * 20 |> clamp 5 20 
        pVy = vy' * 10
    in  (g',{ kind=Debris, x=o.x, y=o.y, vx=pVx,  vy=pVy, radius=o.radius,  sides=4, alive=True, fired=t })

nParticles : Time -> Int -> GameObject o -> StdGen -> [Particle]
nParticles t i e gen = 
    let (newG, newP) = genParticles t gen e
    in case i of
            0 -> []
            otherwise -> newP :: (nParticles t (i-1) e newG)

--COLLISION & BOUNDS CHECKS
checkCollision : GameObject o -> GameObject o -> Bool
checkCollision o1' o2' = 
    let o1 = findVerticies (toFloat o1'.sides) o1'.radius 0 (o1'.x, o1'.y) 
        o2 = findVerticies (toFloat o2'.sides) o2'.radius 0 (o2'.x, o2'.y)
    in collision o1 o2 

outOfBounds : Float -> Float -> GameObject o -> Bool
outOfBounds halfW halfH o = 
    o.x > halfW+100 || o.x < -halfW || 
    o.y > halfH+100 || o.y < -halfH-100 ||
    (not o.alive)

--PHYSICS
genericPhysics : Time -> Float -> GameObject o -> GameObject o
genericPhysics t halfH p = { p | y <- p.y + t * p.vy  |> clamp (-halfH+padding/2) (halfH-hud)
                               , x <- p.x + t * p.vx }

shotPhysics : Time -> Float -> ShotO -> ShotO 
shotPhysics t halfH s  = 
    case s.kind of
        WaveBeam   -> { s | y <- s.y - 2*(sin <| (s.x)/50 ) |> clamp (-halfH) (halfH)
                          , x <- s.x + t * s.vx }
        Debris     -> { s | y <- s.y + t * s.vy * 2 |> clamp (-halfH-100) (halfH+100)
                          , x <- s.x + t * s.vx }
        Bomb       -> { s | x <- s.x + t * s.vx * 50 |> clamp s.x 0 }
        otherwise  -> { s | y <- s.y |> clamp (-halfH) (halfH-hud)
                        , x <- s.x + t * s.vx  }

--MOVEMENT
moveP : Input -> PlayerO -> PlayerO
moveP i p = 
    if p.alive
    then { p | vy <- if | i.shift   -> toFloat <| 6 * i.yDir
                        | otherwise -> toFloat <| 2 * i.yDir 
             , x  <- -(toFloat (fst i.windowSize)/2)+padding }
    else p

--HEALTH & SCORE
healthLost : [EnemyO] -> PlayerO -> PlayerO
healthLost es p = 
    let gotPast = filter (\e -> e.x+e.radius <= p.x) es
        damage  = gotPast |> sum . map (round . (\x-> x.radius/10))
    in { p | health <- if | any (\e -> checkCollision p e) es -> 0
                          | otherwise -> p.health - damage |> clamp 0 100 }

isAlive : PlayerO -> PlayerO
isAlive p = { p | alive <- if | p.health <= 0 -> False
                              | otherwise     -> True }

scoreMod : Int -> [EnemyO] -> Int
scoreMod score es = 
    let numDead = es |> filter (\e -> not e.alive) |> length 
    in score + numDead

--WEAPONS
activeWeapon : [Weapon] -> Weapon
activeWeapon ws = ws |> listToMaybe . filter (\x -> x.active) 
                     |> maybe (head ws) id

swapWeapons : Input -> PlayerO -> PlayerO
swapWeapons i p = 
    let sinceSwapped = (fst i.swap) - (activeWeapon p.weapons).lastSwapped
        toggleWeapon =  map (\w -> { w | active <- not w.active, lastSwapped <- (fst i.swap) })
        okayToSwap   = (snd i.swap) && sinceSwapped > 5
    in { p | weapons <- if | okayToSwap -> toggleWeapon p.weapons
                           | otherwise  -> p.weapons }

shotCanFire : (Time, Bool) -> Weapon -> PlayerO -> [ShotO] -> Bool
shotCanFire (f, k) w p s =
    let lastShot  = s |> listToMaybe |> maybe 0 (\x -> x.fired)
        bombsAway = if w.kind == Bomb then p.bombs > 0 else True
    in p.alive && bombsAway && k && f - lastShot >= w.cooldown 

newShot : Input -> Weapon -> PlayerO -> [ShotO] -> [ShotO]
newShot i w p s = 
    let fireable = if | w.kind /= Bomb -> shotCanFire i.fire w p s
                      | otherwise      -> shotCanFire i.bomb bomb p s
    in if fireable then
      case w.kind of
          Blaster  -> { kind=w.kind, x=p.x+17, y=p.y,    vy=0, vx=5.0, radius=4.0, sides=10, alive=True, fired=(fst i.fire) } :: s
          Bomb     -> { kind=Bomb,   x=p.x,    y=p.y,    vx=1, vy=0,   radius=10,  sides=10,  alive=True, fired=(fst i.bomb) } :: s
          WaveBeam -> { kind=w.kind, x=p.x+17, y=p.y+15, vy=0, vx=8.0, radius=6.0, sides=10,  alive=True, fired=(fst i.fire) } ::
                      { kind=w.kind, x=p.x+17, y=p.y,    vy=0, vx=8.0, radius=6.0, sides=10,  alive=True, fired=(fst i.fire) } ::
                      { kind=w.kind, x=p.x+17, y=p.y-15, vy=0, vx=8.0, radius=6.0, sides=10,  alive=True, fired=(fst i.fire) } :: s
    else s 

shotBomb : Input -> [BombO] -> PlayerO -> PlayerO
shotBomb i bs p = { p | bombs <- if | shotCanFire i.bomb bomb p bs  -> p.bombs - 1
                                    | otherwise -> p.bombs }

explodeBomb : Time -> BombO -> BombO
explodeBomb t b = 
    if (inSeconds t) - (inSeconds b.fired) < 1
    then { b | radius <- b.radius + 4 } 
    else { b | radius <- b.radius - 4
             , alive  <- if | b.radius <= 0 -> False
                            | otherwise -> True }

shotsHit : [ShotO] -> EnemyO -> EnemyO
shotsHit s e = if any (\shot -> checkCollision shot e) s then { e | alive <- False }
               else e

addShots i g = 
    let w = activeWeapon g.player.weapons
    in if shotCanFire i.fire w g.player g.shots then g.totalShots + 1
       else g.totalShots

--STEPPERS
stepPlayer : Input -> GameOs -> PlayerO
stepPlayer i g = g.player |> isAlive . shotBomb i g.bombs . healthLost g.enemies . 
                             genericPhysics i.delta g.halfHeight . moveP i . swapWeapons i

stepEnemies : Input -> GameOs -> [EnemyO]
stepEnemies i g  = 
    let inPlay = filter (\e -> not . outOfBounds g.halfWidth g.halfHeight <| e) g.enemies
        lastC  = inPlay |> listToMaybe |> maybe (enemy1) id
        es'    = if (i.sinceStart - lastC.created >= (100*g.playTime)^(-2))
                 then (fst <| genEnemies i.sinceStart g.halfWidth g.halfHeight g.rGen) :: inPlay 
                 else inPlay
    in map (\x -> genericPhysics i.delta g.halfHeight . shotsHit g.particles . shotsHit g.bombs . shotsHit g.shots <| x) es'

stepWeapons : Input -> GameOs -> [ShotO]
stepWeapons i g = 
    let w = activeWeapon g.player.weapons
        cleanShots = g.shots |> filter (\o -> not . outOfBounds g.halfWidth g.halfHeight <| o) 
                             |> filter (\s -> if s.kind == Blaster 
                                              then not <| any (\e -> checkCollision s e) g.enemies
                                              else s.alive)
    in  cleanShots |> newShot i w g.player
                   |> map (\x -> shotPhysics i.delta g.halfHeight <| x)

stepBombs : Input -> GameOs -> [BombO]
stepBombs i g = 
    let bs' = newShot i bomb g.player g.bombs |> filter (\b -> b.alive)
    in bs' |> map (\b -> shotPhysics i.delta  g.halfHeight <| explodeBomb i.sinceStart <| b)

stepParticles : Input -> GameOs -> [ShotO]
stepParticles i g =
    let currentPs    = filter (\p -> not . outOfBounds g.halfWidth g.halfHeight <| p) g.particles
        deadEs       = filter (\e -> not e.alive) g.enemies
        newParticles = concatMap (\e -> nParticles e.created e.sides e g.rGen) deadEs 
    in newParticles ++ currentPs |> map (\x -> shotPhysics i.delta g.halfHeight <| x)

stepPlayState : Input -> GameOs -> PlayState
stepPlayState i g =
    let togglePause = (snd i.pause) && (fst i.pause) - g.paused > 5
    in if | snd i.pause && g.playState == Start    -> Controls
          | snd i.fire  && g.playState == Start    -> Playing
          | togglePause && g.playState == Controls -> Playing
          | togglePause && g.playState == Paused   -> Playing
          | togglePause && g.playState == Playing  -> Paused
          | not g.player.alive                     -> GameOver
          | otherwise -> g.playState 

stepGame : Input -> GameOs -> GameOs
stepGame i g = 
    let playState' = stepPlayState i g
    in if i.restart && (playState' == Paused || not g.player.alive)
       then { defaultGame | playState <- Playing }
       else case g.playState of
        Playing -> 
                { g | player     <- stepPlayer    i g
                    , enemies    <- stepEnemies   i g
                    , shots      <- stepWeapons   i g
                    , bombs      <- stepBombs     i g
                    , particles  <- stepParticles i g
                    , rGen       <- snd <| genEnemies i.sinceStart g.halfWidth g.halfHeight g.rGen 
                    , score      <- scoreMod g.score g.enemies
                    , totalShots <- addShots i g
                    , playTime   <- g.playTime + (i.delta*timeMod)
                    , playState  <- playState'
                    , paused     <- if g.playState /= playState' then (fst i.pause) else g.paused
                    , halfWidth  <- toFloat (fst i.windowSize)/2
                    , halfHeight <- toFloat (snd i.windowSize)/2 }
        GameOver  -> 
                { g | enemies    <- stepEnemies   i g 
                    , shots      <- stepWeapons   i g
                    , bombs      <- stepBombs     i g
                    , particles  <- stepParticles i g
                    , rGen       <- snd <| genEnemies i.sinceStart g.halfWidth g.halfHeight g.rGen }
        otherwise -> 
                { g | paused     <- if g.playState /= playState' then (fst i.pause) else g.paused
                    , playState  <- playState'}

gameState : Signal GameOs
gameState = foldp stepGame defaultGame input

------ DISPLAY ------
padding = 50
hud     = 50

txt : Color -> String -> Element
txt c = centered . monospace . Text.height 18 . Text.color c . toText 

header : String -> Form
header = toForm . centered . monospace . Text.height 40 . Text.color lightRed . toText 

makeHud : GameOs -> Input -> Form
makeHud g i = 
    let p         = g.player
        equipped  = activeWeapon p.weapons
        lastShot  = g.shots |> listToMaybe |> maybe 0 (\x -> x.fired)
        bombCount = ("●" |> repeat p.bombs |> String.join " ")
        score     = String.padLeft 5 '0' . show <| g.score
        timer     = String.padLeft 5 '0' . show . round . inSeconds <| g.playTime
        paused    = g.playState == Paused
        start     = g.playState == Start
    in group
          [
              "Health: " ++ show p.health ++ "%" |> txt white |> toForm |> move (-g.halfWidth+padding*2, g.halfHeight-padding/2)
            , "Weapon: " ++ show equipped.kind   |> txt equipped.fillColor |> toForm |> move (-g.halfWidth/2.5, g.halfHeight-padding/2)
            , "Bombs: "  ++ bombCount |> txt lightBlue |> toForm |> move (0, g.halfHeight-padding/2)
            , "Time: "   ++ timer     |> txt white |> toForm |> move (g.halfWidth-padding*6, g.halfHeight-padding/2)
            , "Score: "  ++ score     |> txt white |> toForm |> move (g.halfWidth-padding*2, g.halfHeight-padding/2)
            , if paused then makePauseScreen g else spacer 0 0 |> toForm         
            ]

makeStartScreen : GameOs -> Form
makeStartScreen g =
    group [ "
 ██████╗ █████╗ ██████╗ ██████╗ ██╗███╗   ██╗ █████╗ ██╗        ██████╗ ███████╗██╗    ██╗
██╔════╝██╔══██╗██╔══██╗██╔══██╗██║████╗  ██║██╔══██╗██║     ██╗██╔══██╗██╔════╝██║    ██║
██║     ███████║██████╔╝██║  ██║██║██╔██╗ ██║███████║██║     ╚═╝██████╔╝█████╗  ██║ █╗ ██║
██║     ██╔══██║██╔══██╗██║  ██║██║██║╚██╗██║██╔══██║██║     ██╗██╔═══╝ ██╔══╝  ██║███╗██║
╚██████╗██║  ██║██║  ██║██████╔╝██║██║ ╚████║██║  ██║███████╗╚═╝██║     ███████╗╚███╔███╔╝
 ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝╚══════╝   ╚═╝     ╚══════╝ ╚══╝╚══╝ " |> txt lightRed |> toForm |> move (0, padding*4)
          , "Triangalia fell when the [4≤n≤10]gons attacked."      |> txt white |> toForm |> move (0, padding*1.5)
          , "The fleet was all but exterminated."                  |> txt white |> toForm |> move (0, padding) 
          , "You are Cardinal, the last of the Triangle warriors." |> txt white |> toForm |> move (0, 0) 
          , "You have a blaster, a wave beam, and three bombs."    |> txt white |> toForm |> move (0, -padding*0.5)
          , "Don't let them hit you, and don't let them past."     |> txt white |> toForm |> move (0, -padding)
          , "You must hold them off as long as you can."           |> txt white |> toForm |> move (0, -padding*1.5)
          , "Press 'p' to view the controls."                      |> txt white |> toForm |> move (0, -padding*2.5)
          , "Press 'space' to join the battle."                    |> txt white |> toForm |> move (0, -padding*3)
          ]

makeControlsScreen : Form
makeControlsScreen =
    group [ "CONTROLS"                       |> header    |> move (0, padding*2.5)
          , "'p' - to pause"                 |> txt white |> toForm |> move (0, padding*1.5)
          , "'&uarr;&darr;' - to move ship"  |> txt white |> toForm |> move (-padding*4, padding*0.5)
          , "'shift' - to boost speed"       |> txt white |> toForm |> move (-padding*4, -padding*0.5)
          , "'space' - to shoot weapon"      |> txt white |> toForm |> move (padding*4, padding*0.5)
          , "'c' - to change weapons"        |> txt white |> toForm |> move (padding*4, -padding*0.5)
          , "'b' - to deploy a bomb"         |> txt white |> toForm |> move (0, -padding*1.5)
          , "Press 'p' to begin."            |> txt white |> toForm |> move (0, -padding*2.5)
          ]

makePauseScreen : GameOs -> Form
makePauseScreen g =
    group [ "PAUSED"                         |> header |> move (0, padding*2.5)
          , "'p' - to resume"                |> txt white |> toForm |> move (0, padding*1.5)
          , "'r' - to restart game"          |> txt white |> toForm |> move (0, padding)
          , "'&uarr;&darr;' - to move ship"  |> txt white |> toForm |> move (-padding*4, 0)
          , "'shift' - to boost speed"       |> txt white |> toForm |> move (-padding*4, -padding*0.5)
          , "'space' - to shoot weapon"      |> txt white |> toForm |> move (padding*4, 0)
          , "'c' - to change weapons"        |> txt white |> toForm |> move (padding*4, -padding*0.5)
          , "'b' - to deploy a bomb"         |> txt white |> toForm |> move (0, -padding*1.5)
          ]

makeDeathScreen : GameOs -> Form
makeDeathScreen g =
    let finalScore = g.score      |> String.padLeft 5 '0' . show
        totalShots = g.totalShots |> String.padLeft 5 '0' . show 
        finalTime  = g.playTime   |> String.padLeft 5 '0' . String.left 4 . show . inSeconds
        accuracy   = max 0 (100.0 * (toFloat g.score)/(toFloat g.totalShots)) |> String.left 5 . show
    in 
    group [ "GAME OVER"                                  |> txt lightRed |> toForm |> move (0, padding*3.5)
          , "Survival Time: " ++ finalTime ++ " seconds" |> txt white |> toForm |> move (0, padding*1.5)
          , "Final Score:  " ++ finalScore               |> txt white |> toForm |> move (0, padding*0.5)
          , "Total Shots:  " ++ totalShots               |> txt white |> toForm |> move (0, -padding*0.5)
          , "Accuracy:     " ++  accuracy  ++ "%"        |> txt white |> toForm |> move (0, -padding*1.5)
          , "Press 'r' to play again!"                   |> txt white |> toForm |> move (0, -padding*2.5)
          ]

makePlayer : PlayerO -> Form
makePlayer p = 
    if p.alive then
        ngon p.sides p.radius   |> outlined (solid white)
                                |> move (p.x, p.y)
    else spacer 0 0 |> toForm

makeShots : ShotO -> Form
makeShots s = 
    let fill = if | s.kind == Blaster  -> lightRed
                  | s.kind == WaveBeam -> lightPurple
                  | otherwise          -> lightBlue
    in circle s.radius |> outlined (solid fill)
                       |> move (s.x, s.y)

makeBombs : BombO -> Form
makeBombs b =
    circle b.radius |> outlined (solid lightBlue)
                    |> move (b.x, b.y)

makeEnemies : EnemyO -> Form
makeEnemies e = if e.alive then 
    ngon e.sides e.radius |> outlined (solid white)
                          |> move (e.x, e.y)
    else spacer 0 0 |> toForm

makeParticles : Particle -> Form
makeParticles p = 
    rect p.radius 2 |> filled yellow
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
              , case g.playState of 
                      Start     -> makeStartScreen g
                      Controls  -> makeControlsScreen
                      GameOver  -> makeDeathScreen g
                      otherwise -> makeHud g i 
              ,  if g.playState == Playing then makePlayer g.player else spacer 0 0 |> toForm ]
              , map makeShots g.shots
              , map makeEnemies g.enemies
              , map makeBombs g.bombs
              , map makeParticles g.particles
              ] 

main = lift3 display Window.dimensions gameState input
