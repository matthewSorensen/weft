{-# LANGUAGE TupleSections #-}
module Geometry.Polygon where

import Robotics.Thingomatic.Points 
import Data.Vector hiding (concat,empty,reverse,foldl,head)
import Prelude hiding (map,scanl,tail,maximum,minimum)
import Data.List (sortBy,reverse)
import Data.Function (on)
import Data.Set (Set,empty,insert,elems,delete)
import qualified Data.List as L


data Polyline = Poly Point2 (Vector Point2)
type Index = Int
type Range = (Double,Double)
type Action = Int->Set Int->Set Int

project::Point2->Polyline->Vector (Range,[Int])
project basis = fromList . reverse . overlap . sortBy order . tag . toIntervals basis
  where toIntervals i (Poly d p) = tail $ scanl (\(_,cur) len -> (cur,len+cur)) (0.0, i<.>d) $ map (i<.>) p
        tag = concat . toList . imap tagSegment
        tagSegment index (start,end) 
          | start == end = []
          | otherwise    = [(insert,(index,min start end)),(delete,(index,max start end))]
        order = compare `on` (snd . snd)         
        overlap [] = []
        overlap x  = fst $ foldl overlap' (init x) x
            where init = ([],) . (empty,) . snd . snd . head
                  overlap' (acc,(set,lower)) (action,(index,upper))
                    | lower == upper = (acc,(action index set,lower))
                    | otherwise      = let seg = ((lower,upper),elems set)
                                       in (seg:acc,(action index set,upper))
                                          
                                          
rasterize::Point2->Double->Polyline->[Point2]
rasterize basis width poly = let proj = project basis poly
                                 minX = minimum $ map (fst.fst) proj
                                 maxX = maximum $ map (snd.fst) proj
                             in []
                           