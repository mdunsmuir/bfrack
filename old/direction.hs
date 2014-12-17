import Data.List

data Direction = DirStraight | DirLeft | DirRight deriving Show

data Point = Point {
  x_of :: Double,
  y_of :: Double
} deriving Show

pointSub :: Point -> Point -> Point
pointSub p1 p2 = Point ((x_of p1) - (x_of p2)) ((y_of p1) - (y_of p2))

cornerDirection :: Point -> Point -> Point -> Direction
cornerDirection p1 p2 p3 = 
  let
    p1s = pointSub p1 p2
    p3s = pointSub p3 p2
    p1sang = (-(atan2 (y_of p1s) (x_of p1s)))
    p3y = ((x_of p3s) * (sin p1sang)) + ((y_of p3s) * (cos p1sang))
  in
    case compare p3y 0.0 of
      GT -> DirRight
      LT -> DirLeft
      EQ -> DirStraight

listDirections :: [Point] -> [Direction]
listDirections (x:y:z:xs) = (cornerDirection x y z) : (listDirections (y:z:xs)) 
listDirections _ = []

grahamHull :: [Point] -> [Point]
grahamHull list | length list > 2 = hullIter finalOrdered
  where
    -- sort all points by y, set p = lowest one
    compareY p1 p2 = compare (y_of p1) (y_of p2)
    ySorted = sortBy compareY list
    p = head ySorted

    -- sort the rest by their angle with p relative to x-axis
    angleOf point = atan2 (y_of normalized) (x_of normalized)
      where normalized = pointSub point p
    compareAngle p1 p2 = compare (angleOf p1) (angleOf p2)
    angleSorted = sortBy compareAngle (tail ySorted)

    -- get ordered list for final iteration
    finalOrdered = p : angleSorted ++ [p]

    hullIter (p1:p2:p3:ps) | length ps > 0 =
      case dir of 
        DirLeft -> 
          case leadingDir (p1:restOfList) of 
            DirLeft -> p1 : (hullIter restOfList)
            _ -> p1 : (tail restOfList)
          where
            leadingDir (q1:q2:q3:qs) = cornerDirection q1 q2 q3
            restOfList = hullIter (p2:p3:ps)
        _ -> p1 : p3 : ps
      where dir = cornerDirection p1 p2 p3

    hullIter [p1,p2,p3] =
      case cornerDirection p1 p2 p3 of
        DirLeft -> p1:p2:p3:[]
        _ -> [p1, p3]

    hullIter x = x
    
grahamHull _ = []
