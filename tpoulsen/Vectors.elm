module Tpoulsen.Vectors where

import Tpoulsen.Lib (listToMaybe)

--VECTOR STUFF
type Vector2 = (Float,Float)
type Vertex  = (Float,Float)
type Axis    = ((Float,Float),(Float,Float))

lineToVector2 : ((Float,Float),(Float,Float)) -> Vector2
lineToVector2 ((x1,y1), (x2,y2)) = (x2-x1,y2-y1)

add : Vector2 -> Vector2 -> Vector2
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

subtract : Vector2 -> Vector2 -> Vector2
subtract (x1,y1) (x2,y2) = (x1-x2,y1-y2)

magnitude : Vector2 -> Float
magnitude (x1,y1) = sqrt <| (x1)^2 + (y1)^2

normalize : Vector2 -> Vector2
normalize ((x1,y1) as v) = 
    let mag = magnitude v
    in (x1/mag,y1/mag)

--Negative means moving away from each other; positive towards.
dotProduct : Vector2 -> Vector2 -> number
dotProduct (x1,y1) (x2,y2) = x1*x2 + y1*y2

--Projecting v1 onto v2.
projection : Vector2 -> Vector2 -> Vector2
projection ((x1,y1) as v1) ((x2,y2) as v2) = 
    let dp = dotProduct v1 v2
        x' = (dp / (x2^2 + y2^2)) * x2
        y' = (dp / (x2^2 + y2^2)) * y2 
    in (x',y')

rightNormal : Vector2 -> Vector2
rightNormal (x1,y1) = (-y1,x1)

leftNormal : Vector2 -> Vector2
leftNormal (x1,y1) = (y1,-x1)

perProduct : Vector2 -> Vector2 -> number
perProduct v1 v2 = dotProduct v1 <| rightNormal v2

--Separating Axis Theorem implementation

--Test axes are the normals of the shapes being tested.
--Assumed that [Vertex] is clockwise.
getAxes : [Vertex] -> [Vector2]
getAxes vs = 
    let vs' = vs ++ [head vs] ++ [head . tail <| vs]
    in map leftNormal <| constructAxes vs'

constructAxes : [Vertex] -> [Vector2]
constructAxes ((x::y::zs) as vs) = 
   case zs of
        []        -> []
        otherwise -> lineToVector2 (x, y) :: constructAxes (tail vs)

--Project all of a shapes verticies onto the axis of interest.
projectShape : [Vertex] -> Vector2 -> (number,number)
projectShape vs a =
    let dps = map (flip dotProduct (a)) vs
    in (minimum dps, maximum dps)

--If True, then overlapping.
overlap : Axis -> Bool
overlap ((min1,max1),(min2,max2)) = not <| (min1 > max2) || (min2 > max1)

overlapMagnitude : Axis -> Float
overlapMagnitude ((min1,max1),(min2,max2)) =
    minimum <| [max1-min2, max2-min1]

--Is there overlap between the projections on all of the axes?
collision : [Vertex] -> [Vertex] -> Bool
collision s1 s2 =
    let axes = getAxes s1 ++ getAxes s2
        s1Proj = map (projectShape s1) <| axes
        s2Proj = map (projectShape s2) <| axes
        zippedProjections = zip s1Proj s2Proj
    in all overlap zippedProjections

collisionMTV : [Vertex] -> [Vertex] -> ((Vector2),Float)
collisionMTV s1 s2 =
    let axes = map normalize <| getAxes s1 ++ getAxes s2
        s1Proj = map (projectShape s1) <| axes
        s2Proj = map (projectShape s2) <| axes
        projectionsAndAxes = zip axes  <| zip s1Proj s2Proj
        overlapping = filter (overlap . snd) projectionsAndAxes
        minOverlap  = maybe ((0,0),0) id . listToMaybe . sortBy snd . map (\x -> (fst x, overlapMagnitude . snd <| x))
    in minOverlap overlapping

