module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidVolume
, cuboidArea
) where


sphereVolume :: Flaot -> Float
sphereVolume radius  = (4.0/3.0) * pi * (radius ^ 3)

sphereArea :: Flaot -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Flaot -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Flaot -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Flaot -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Flaot -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b