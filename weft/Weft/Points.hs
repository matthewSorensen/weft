{-# LANGUAGE TypeSynonymInstances #-}
module Weft.Points 
       (Point3,Point2,Point (..)) where

type Point3 = (Double,Double,Double)
type Point2 = (Double,Double)

class Point a where
  (<+>)::a->a->a
  (<*>)::Double->a->a
  (<.>)::a->a->Double
  (<->)::a->a->a
  lift3::a->Point3->Point3
  lift3Rel::a->Point3->Point3
  
instance Point Point3 where
  (x,y,z)<+>(x',y',z') = (x+x',y+y',z+z') 
  a <*> (x,y,z) = (a*x,a*y,a*z)
  (x,y,z) <.> (x',y',z') = (z*z')+(y*y')+(x*x')
  (x,y,z) <-> (x',y',z') = (x-x',y-y',z-z')
  lift3 = const
  lift3Rel = (<+>)

instance Point Point2 where
  (x,y)<+>(x',y') = (x+x',y+y') 
  a <*> (x,y) = (a*x,a*y)
  (x,y) <.> (x',y') = (y*y')+(x*x')
  (x,y) <-> (x',y') = (x-x',y-y')
  lift3 (x,y) (_,_,z) = (x,y,z)
  lift3Rel (x,y) a = a <+> (x,y,0)