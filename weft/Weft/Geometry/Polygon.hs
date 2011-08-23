module Weft.Geometry.Polygon where

import Weft.Points 
import Data.Vector hiding (concat,empty,reverse)
import Prelude hiding (map,scanl,tail)
import Data.List (sortBy,reverse)
import Data.Set (Set,empty,insert,elems,delete)
import qualified Data.List as L


data Polyline = Poly Point2 (Vector Point2)
type Index = Int
type Range = (Double,Double)
type Action = Int->Set Int->Set Int

project::Point2->Polyline->[(Range,[Int])]
project basis = overlap . sortBy order . tag . toIntervals basis
  where toIntervals i (Poly d p) = tail $ scanl (\(_,cur) len -> (cur,len+cur)) (0.0, i<.>d) $ map (i<.>) p
        tag = concat . toList . imap tagSegment
        tagSegment index (start,end) 
          | start == end = []
          | otherwise    = [(insert,index,min start end),(delete,index,max start end)]
        order (_,_,a) (_,_,b) = a `compare` b         
        
overlap::[(Action,Int,Double)]->[(Range,[Int])]
overlap [] = []
overlap x  = overlap' (init $ x !! 0) x
  where init (_,_,x) = ([],empty,x)
        overlap' (acc,_,_) [] = reverse acc
        overlap' (acc,set,lower) ((action,index,upper):rest)
          | lower == upper = overlap' (acc,action index set,lower) rest
          | otherwise      = overlap' (((lower,upper),elems set):acc,action index set,upper) rest
