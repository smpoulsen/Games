{-Makes a tile-map based on the gameMap nested array.-}
import Keyboard
import Window

--INPUT

type Input = { x:Int, y:Int, delta:Time }

delta : Signal Time
delta = lift (\t -> t/20) (fps 60)

input : Signal Input
input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ lift .y Keyboard.arrows
                               ~ delta)


--MODEL
type Tile    = { walkability:Float, coords:(Float, Float), dimensions:(Float,Float), repr:Form }
type GameMap = [Tile]

type Game    = { level:GameMap, player:Player }
type Player  = { x:Float, y:Float, w:Float, h:Float }

defaultGame = { level=defaultMap, player=defaultPlayer }

defaultPlayer : Player
defaultPlayer = { x=0.0, y=-100.0, w=15, h=15 }
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

--Collision checks aren't perfect; everything assumed to be roughly circular.
{-
collision : Tile -> Player -> Bool
collision o1 o2 = (o1.x + o1.radius >= o2.x - o2.radius && 
                   o1.x - o1.radius <= o2.x + o2.radius) &&
                  (o1.y + o1.radius >= o2.y - o2.radius && 
                   o1.y - o1.radius <= o2.y + o2.radius)
-}
outOfBounds : Float -> Float -> Player -> Bool
outOfBounds halfW halfH o = 
    o.x + (o.w/2) > halfW || o.x - (o.w/2) < -halfW || 
    o.y + (o.h/2) > halfH || o.y - (o.h/2) < -halfH-100

euclideanDist : (number,number) -> (number,number) -> Float
euclideanDist (x1,y1) (x2,y2) = sqrt <| (x2-x1)^2 + (y2-y1)^2

movingTowards : Tile -> Player -> (Float, Float) -> Bool
movingTowards t p (x,y) = 
    let dist  = euclideanDist (.coords t) (p.x, p.y)
        dist' = euclideanDist (.coords t) (p.x+x, p.y+y)
    in dist' < dist 

standingOn : GameMap -> Player -> Tile
standingOn m p = 
    let allCoords = map .coords m
    in fst . head . sortBy snd . zip m <| map (euclideanDist (p.x,p.y)) allCoords

walk : Input -> Tile -> Player -> Player
walk i t p = 
    let (x',y') = (toFloat i.x, toFloat i.y)
    in case t.walkability of
        0 ->         { p | x <- if | not (movingTowards t p (x',y')) -> p.x + x' |> clamp -300 300
                                   | otherwise -> p.x 
                         , y <- if | not (movingTowards t p (x',y')) -> p.y + y' |> clamp -300 300
                                   | otherwise -> p.y }
        otherwise -> { p | x <- p.x + (toFloat i.x)*t.walkability/2 |> clamp -300 300
                         , y <- p.y + (toFloat i.y)*t.walkability/2 |> clamp -300 300 }

stepPlayer : Input -> GameMap -> Player -> Player
stepPlayer i m p = p |> walk i (standingOn m p)

stepGame : Input -> Game -> Game
stepGame i g = { g | player <- stepPlayer i g.level g.player }

gameState : Signal Game
gameState = foldp stepGame defaultGame input



(mainWidth, mainHeight) = (600, 600)
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
    , repr        = rect 50 50 |> filled blue
                               |> move (originX+x, originY-y)
    }

makeGrass : Float -> Float -> Tile
makeGrass x y =
    { walkability = 10
    , coords      = (originX+x, originY-y)
    , dimensions  = (50,50)
    , repr        = rect 50 50 |> filled green
                               |> move (originX+x, originY-y)
    }

makeSand : Float -> Float -> Tile
makeSand x y = 
    { walkability = 2
    , coords      = (originX+x, originY-y)
    , dimensions  = (50,50)
    , repr        = rect 50 50 |> filled brown
                               |> move (originX+x, originY-y)
    }

makePlayer : Player -> Form
makePlayer p =
    rect p.h p.w |> filled white
                 |> move (p.x, p.y)

display : (Int, Int) -> Game -> Element
display (w,h) g =
    let playMap = defaultMap
    in container w h middle <| collage mainWidth mainHeight <|
        concat [ map .repr playMap
               , [makePlayer g.player]
               , [(g.player.x, g.player.y) |> asText |> toForm ]
        --, [playMap |> asText |> toForm]
        ]

main = lift2 display Window.dimensions gameState
