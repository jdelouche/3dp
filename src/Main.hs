{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Graphics.Implicit
import Graphics.Implicit.Primitives


out = unionR 14 [rectR 0 (-40,-40) (40,40),
        translate (40,40) (circle 30) ]

outStl = union [
    rect3R 0 (0,0,0) (20,20,20),
    translate (20,20,20) (sphere 15) ]


outStl2 = unionR 5 [
    rect3R 3 (0,0,0) (20,20,20),
    translate (20,20,20) (sphere 5),
    translate (20,0,20) (sphere 5),
    translate (0,20,20) (sphere 5),
    translate (20,20,0) (sphere 5),

    translate (0,0,0) (sphere 5),
    translate (0,20,0) (sphere 5),
    translate (20,0,0) (sphere 5),
    translate (0,0,20) (sphere 5)
 ]

outStl3 = union [
    translate (0,0,0) (sphere 10),
    translate (0,0,20) (sphere 10),
    translate (0,0,40) (sphere 10),
    translate (0,0,60) (sphere 10)
     ]


outStl4 =
  union [
    rectR 0 (-40,-40) (40,40)
  ]

outStl6 = difference [ difference [translate (0,0,0) (sphere 20),
                                   translate (0,0,0) (sphere 10)],
                 translate (0,0,0) (rect3R 0 (0,0,0) (40,40,40))]

halfBall x c = rotate3 (0, pi, 0) (
               difference [
                            difference [translate (0,0,0) (sphere x),
                                        translate (0,0,0) (sphere (x/c))],
                            translate (0,0,0) (rect3R 0 (-2*x,-2*x,0) (2*x,2*x,2*x))])

outStl14 = halfBall 20 2

outStl15 = halfBall 10 2

fourthBall x c = difference [halfBall x c, translate (0,0,0) (rect3R 0 (0,-2*x,-2*x) (2*x,2*x,2*x))]

peggedHalfBallIn x r h l = rotate3 (0, pi, 0)
               (
               difference [
                      difference [
                            translate (0,0,0) (sphere x),
                            translate (0,0,0) (rect3R 0 (-2*x,-2*x,0) (2*x,2*x,2*x))],
                      translate (0,l,-h) (cylinder r (2*h)),
                      translate (0,-l,-h) (cylinder r (2*h))
                     ]
               )

peggedHalfBallOut x r h l = rotate3 (0, pi, 0)
               (
               union [
                      difference [
                            translate (0,0,0) (sphere x),
                            translate (0,0,0) (rect3R 0 (-2*x,-2*x,0) (2*x,2*x,2*x))],
                      translate (0,l,0) (cylinder r h),
                      translate (0,-l,0) (cylinder r h)
                     ]
               )

outStl16 = fourthBall 20 (20/12)

outStl17 = peggedHalfBallIn 20 2 10 10

outStl18 = union [ peggedHalfBallOut 20 2 10 10,
                   rotate3 (0, pi, 0) (translate (0,0,50) (peggedHalfBallIn 20 2 10 10))]

obj2d_1 :: SymbolicObj2
obj2d_1 =
	unionR 10
		[ circle 10
		, translate (22,0) $ circle 10
		, translate (0,22) $ circle 10
		, translate (-22,0) $ circle 10
		, translate (0,-22) $ circle 10
		]

obj2d_2 = rectR 5 (-20,-20) (20,20)

w=10
l=w
a = w/2
b = a+w
c = 50/w
dH = 3*w

twistIt h = (3/2)*c*h
scaleIt h = 1/2 - h/dH
moveIt  h = (0,0)

twistIt2 h = c*h
scaleIt2 h = (dH - h)/dH
moveIt2  h = (0,0)

object1 :: SymbolicObj3
object1 = extrudeRM 0 (Just twistIt) (Just scaleIt) (Just moveIt) obj2d_2 (Left 80)

obj2d_3_1 = translate (-b,-b) $ rectR 0 (0,0) (w,l)
obj2d_3_2 = translate (a,a)   $ rectR 0 (0,0) (w,l)
obj2d_3_3 = translate (-b,a)  $ rectR 0 (0,0) (w,l)
obj2d_3_4 = translate (a,-b)  $ rectR 0 (0,0) (w,l)

object3_1 :: SymbolicObj3
object3_1 = extrudeRM 0 (Just twistIt) (Just scaleIt) (Just moveIt) obj2d_3_1 (Left dH)
object3_2 :: SymbolicObj3
object3_2 = extrudeRM 0 (Just twistIt) (Just scaleIt) (Just moveIt) obj2d_3_2 (Left dH)
object3_3 :: SymbolicObj3
object3_3 = extrudeRM 0 (Just twistIt) (Just scaleIt) (Just moveIt) obj2d_3_3 (Left dH)
object3_4 :: SymbolicObj3
object3_4 = extrudeRM 0 (Just twistIt) (Just scaleIt) (Just moveIt) obj2d_3_4 (Left dH)

sphereH = translate (0,0,dH/2) $ sphere (w/5)

cubeL = translate (0,0,0) $ scale (3*w,3*w,1) $ rect3R 0 (-1/2,-1/2,-1/2) (1/2,1/2,1/2)
cubeH = translate (0,0,dH) $ scale (3*w,3*w,1) $ rect3R 0 (-1/2,-1/2,-1/2) (1/2,1/2,1/2)

outStl9 = union [object3_1, object3_2, object3_3, object3_4]

outStl10 = union [cubeL, object3_1, sphereH]

outStl11 = union [cubeL, object3_1, object3_2, object3_3, object3_4, sphereH]

outStl13 = union [cubeL, object3_1, object3_2, object3_3, object3_4, sphereH, cubeH]

fl = 2
p = 1
hH=fl*10
dD=5
ag=fl*360
square p = rectR 0 (-p,-p) (p,p)
triangle p = polygon [ (0,-p), (0,p), (p,0) ]
triangleEqui p = polygon [ (0,-p*(sqrt 3)/2), (0,p*(sqrt 3)/2), (p,0) ]
filetS1 = rotateExtrude (ag) (Nothing) (Left (0,hH)) (Left 0) (translate (dD,dD) (square p))

filetS2 = rotate3 (0,0,pi/2) filetS1
filetS3 = rotate3 (0,0,pi) filetS1
filetS4 = rotate3 (0,0,3*pi/2) filetS1

cube dD = rect3R 0 (-2*dD,-2*dD,-dD) (2*dD,2*dD,dD)

topCube hH dD = translate (0,0,hH+dD) (cube dD)

bottomCube dD = translate (0,0,-dD) (cube dD)

cylForSquare = cylinder (dD-p) hH

cylForTriangle dD hH = cylinder dD hH

outStl19 = difference[union[cylForSquare,
                            filetS1,filetS2,filetS3,filetS4],
                            bottomCube dD,topCube hH dD ]

filet ag hH dD shape = translate (0,0,-dD)
                       (rotateExtrude (ag) (Nothing) (Left (0,hH+dD/2)) (Left 0)
                       (translate (dD,dD) (shape)))


shaft ag hH dD shape = difference[
                            union[
                                    filetT1
                                    ,cylForTriangle dD hH
                                  ]
                                    ,bottomCube dD
                                    ,topCube hH dD
                                  ]
                                  where filetT1 = filet ag hH dD shape
                                        filetT2 = rotate3 (0,0,pi/2)   filetT1
                                        filetT3 = rotate3 (0,0,pi)     filetT1
                                        filetT4 = rotate3 (0,0,3*pi/2) filetT1

screwHead2D w = translate (0,0) $ polygon $ (headH w) >>= (\x -> [polarToDescartes x])

polarToDescartes (ro, teta) = (ro*(sin teta),ro*(cos teta))
distance (x1,y1) (x2,y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

triangleEquiBis h = rotate (pi/6) $ polygon $ equilateral h >>= (\x -> [polarToDescartes x])
equilateral ro = [(ro,teta1),(ro,teta2),(ro,teta3)]
            where teta1=0
                  teta2=2*pi/3
                  teta3 = -teta2
hexagon ro = [(ro,teta1),(ro,teta2),(ro,teta3),(ro,teta4),(ro,teta5),(ro,teta6)]
            where teta1 = 0
                  teta2 = pi/3
                  teta3 = 2*teta2
                  teta4 = pi
                  teta5 = -teta3
                  teta6 = -teta2

widthHead ro = distance (polarToDescartes (ro,teta1)) (polarToDescartes (ro,teta3))
                                        where teta1 = 0
                                              teta2 = pi/3
                                              teta3 = 2*teta2

headH h = hexagon ro where ro = last [x/k|x<-[0..k*h], widthHead x <= k*h]
                                  where k =10

screwHead w h = translate (0,0,-h) $
                extrudeRM 0 (Nothing) (Nothing) (Nothing) (screwHead2D w) (Left h)

wholeBolt w h = translate (0,0,h) $
                extrudeRM 0 (Nothing) (Nothing) (Nothing) (screwHead2D w) (Left h)

screw k l d w h p =
                         union [
                          shaft ((b + (b - 1)*(1-k)/3)*l/10*3/2*360) l d (triangleEqui p)
                         ,screwHead w h
                         ]

                         where b = 3.5

hollowScrew k l d w h p =
                             difference [
                             screw k l d w h p
                            ,translate (0,0,-h) (cylinder (3*d/4) (l+h))
                            ]

bolt k d w h p = difference [
                 wholeBolt (1.4*w) (h)
                ,screw k (4*h) d w h p
                ]

torus dD shape = translate (0,0,0)
                       (rotateExtrude 180 (Nothing) (Left (0,0)) (Left 0)
                       (translate (dD,0) (shape)))

cage d c = torus d (square c)

way d c = torus d (circle c)

split d c = torus d (rectR 0 (-c/10,-3*c) (c/10,3*c))

wheel d c = difference [ cage d (1.5*c)
                       , way d c
                       , split d c
                       ,  halfCutter d c ]

halfCutter d c = rect3R 0 (0,-(d+3*c),-2.1*c) ((d+3*c),(d+3*c),2.1*c)

ball d c teta = translate (fst p, snd p, 0) (peggedBall c) where p = polarToDescartes (d,teta)

pos n teta = map ((*) teta) [1..n]

balls d c n teta = union (map (ball d c) (pos n teta))

outerRing d c = torus (d+c) (translate (c/3,0) (rectR 0 (-c/2,-3*c/2) (c/2,3*c/2)))
ballsWay d c = torus d (circle (1.1*c))
outerCage d c = difference [outerRing d c, ballsWay d c]

innerRing d c = torus (d-c) (translate (-c/3,0) (rectR 0 (-c/2,-3*c/2) (c/2,3*c/2)))
innerCage d c = difference [innerRing d c, ballsWay d c]

middleRing d c = torus d (translate (0,0) (rectR 0 (-6*c/10,-3*c/2) (6*c/10,3*c/2)))
middleCage d c n teta = difference [middleRing d c, balls d (1.1*c) n teta]

ballBearing d c n = union [outerCage d c
                          ,balls d c n teta
                          ,innerCage d c
                          ,middleCage d c n teta] where teta = 2*pi/n

halfBallBearing d c n = difference [ballBearing d c n
                                   ,rotate3 (0,0,-pi/24) (halfCutter d c)
                                   ,rotate3 (0,0,3*pi/4) (halfCutter d c)
                                   --,rotate3 (0,0,0) (halfCutter d c)
                                   ]

peggedBall c = union [ sphere c, translate (0,0,-1.5*c) (cylinder (c/10) (3*c))]

-- MAIN
main = writeSTL 0.25 "peggedBall.stl" (halfBallBearing 40 5 20)



