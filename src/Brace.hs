module Brace where

import Utils

import Control.Arrow (second)
import Graphics.ToyFramework
import qualified Graphics.Rendering.Cairo as C

drawBrace l r = drawSVGD svgd
 where
  drawSVGD [] = return ()
  drawSVGD ((_, []):xs) = drawSVGD xs
  drawSVGD (('m', (pnt:pnts)):xs) = relMove pnt >> drawSVGD (('l', pnts) : xs)
  drawSVGD (('M', (pnt:pnts)):xs) = move    pnt >> drawSVGD (('L', pnts) : xs)
  drawSVGD (('l', ((x,y):pnts)):xs) = C.relLineTo x y >> drawSVGD (('l', pnts):xs)
  drawSVGD (('L', ((x,y):pnts)):xs) = C.lineTo    x y >> drawSVGD (('L', pnts):xs)
  drawSVGD (('c', ((x1,y1):(x2,y2):(x3,y3):pnts)):xs) = C.relCurveTo x1 y1 x2 y2 x3 y3
                                                     >> drawSVGD (('c', pnts):xs)
  drawSVGD (('C', ((x1,y1):(x2,y2):(x3,y3):pnts)):xs) = C.curveTo x1 y1 x2 y2 x3 y3
                                                     >> drawSVGD (('C', pnts):xs)
  drawSVGD r = return . (const ()) $ debug' "WARNING unrecognized drawSVGD: " r
  --TODO: h H v V z Z
  svgd = map (second $ map (\(x,y) -> (y, negate x)))
    [ ('m', [ (7.3539846,30.857258 + r - l) ])
    , ('c', [ (-2.6279614,-1.858148)
            , (-2.45455,-3.17612)
            , (-2.45455,-12.36515-r)
            , (0,-8.51208-r)
            , (-0.29247,-10.2824-r)
            , (-2.07258,-12.54545-r) ])
    , ('l', [ (-2.07258003,-2.63486)
            , (2.07258003,-2.63486) ])
    , ('c', [ (1.76401,-2.24257)
            , (2.07258,-4.03897)
            , (2.07258,l-12.0657401)
            , (0,l-10.0066601)
            , (0.74268,l-12.5561301)
            , (4.19606,l-14.4043302) ])
    , ('l', [ (2.1502304,-1.1507699)
            , (-2.1731404,2.7627099) ])
    , ('c', [ (-1.89269,2.4061601)
            , (-2.17315,4.01848006)
            , (-2.17315,12.4929902-l)
            , (0,8.3296701-l)
            , (-0.29834,10.1095501-l)
            , (-2.07258,12.3651401-l) ])
    , ('l', [ (-2.07258,2.63486)
            , (2.07258,2.63486) ])
    , ('c', [ (1.77424,2.25559)
            , (2.07258,4.03547)
            , (2.07258,r+12.36514)
            , (0,r+8.32967)
            , (0.29834,r+10.10955)
            , (2.07258,r+12.36514)
            , (2.6093504,3.31725)
            , (2.7710974,3.283733)
            , (-1.61803,0.18032) ])
    , ('z', [])]
