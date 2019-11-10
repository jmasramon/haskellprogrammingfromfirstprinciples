module Route where

import Data.List.Split

data Lane = AA | BB deriving (Show, Eq)

data Section = Section { a :: Integer
                     , b :: Integer
                     , c :: Integer
              } deriving (Show)

type RoadSystem = [Section]

type Route = (String, Integer)
type ExtRoute = (String, Integer, Integer)

data Routes = Routes { routeA :: Route
                     , routeB :: Route
                     } deriving (Show)

instance Semigroup Routes where
  (<>)  (Routes (sa1, ia1) (sb1, ib1)) 
        (Routes (sa2, ia2) (sb2, ib2)) = 
    Routes ((sa1 ++ sa2), (ia1 + ia2)) ((sb1 ++ sb2), (ib1 + ib2))

data AltRouts = AltRouts { routA :: Route
                          , routB :: Route
                          , c' :: Integer
                          } deriving (Show)

fromSectionList :: [[Integer]] -> RoadSystem
fromSectionList intList = map toSection intList
  where toSection [a, b, c] = Section a b c

fastAlternatives :: Section -> Routes
fastAlternatives (Section a b c) = Routes routeA routeB
  where routeA = if a < aInd 
                 then ("a", a)
                 else ("bc", aInd)
        routeB = if b < bInd 
                  then ("b", b)
                  else ("ac", bInd)
        aInd = b + c
        bInd = a + c

fastAltRoutes :: Section -> AltRouts
fastAltRoutes (Section a b c) = AltRouts routeA routeB c
  where routeA = if a < aInd 
                 then ("a", a)
                 else ("bc", aInd)
        routeB = if b < bInd 
                  then ("b", b)
                  else ("ac", bInd)
        aInd = b + c
        bInd = a + c

bestPathOptions :: Routes -> Route
bestPathOptions (Routes (sa, ia) (sb, ib)) = if ia < ib 
                                            then (sa, ia)
                                            else (sb, ib)

bestAltRoutes :: AltRouts -> ExtRoute
bestAltRoutes (AltRouts (sa, ia) (sb, ib) c) = if ia < ib 
                                            then (sa, ia, c)
                                            else (sb, ib, c)                      

straightPaths :: RoadSystem -> Routes                                     
straightPaths roadSystem = foldl (aggFastAlts) (Routes ("", 0) ("", 0)) roadSystem where
  aggFastAlts acc nv = acc <> fastAlternatives nv

globalFasterAlternative :: [Routes] -> Route
globalFasterAlternative routes = foldl (aggFastAlts) ("", 0) routes where
  aggFastAlts (accS, accI) nv = (accS ++ nvS, accI + nvI)   
   where (nvS, nvI) = bestPathOptions nv

globalFasterAlternative' :: [AltRouts] -> ExtRoute
globalFasterAlternative' altRoutes = foldl (aggFastAlts) ("", 0, 0) altRoutes where
  aggFastAlts (accS, accI, accC) nv = if (curLane == lastLane)
                                then (accS ++ nvS, accI + nvI, c)   
                                else (accS ++ "c" ++ nvS, accI + accC + nvI, c)
    where (nvS, nvI, c) = bestAltRoutes nv
          lastLane = if (accS == "" || last accS == 'a')
                      then AA
                      else BB
          curLane = if (head nvS == 'a')
                    then AA
                    else BB

globalFasterAlternative'' :: [AltRouts] -> Route
globalFasterAlternative'' altRouts = (rS, rI) 
  where (rS, rI, cI) = globalFasterAlternative' altRouts

road :: [Integer]
-- road = [50,10,30]
-- road = [50,10,30,5,90,20]
-- road = [50,10,30,5,90,20,40,2,25]
road = [50,10,30,5,90,20,40,2,25,10,8,0]

roadChuncks::[[Integer]]
roadChuncks = chunksOf 3 road

roadSystem :: RoadSystem
roadSystem = fromSectionList roadChuncks

routes :: [Routes]
routes = map fastAlternatives roadSystem

altRoutes :: [AltRouts]
altRoutes = map fastAltRoutes roadSystem

straights :: Routes
straights = straightPaths roadSystem

bestRoutes :: [Route]                                            
bestRoutes = map bestPathOptions routes                       
          
bestAltRtes :: [ExtRoute]                                            
bestAltRtes = map bestAltRoutes altRoutes                       
          
bestRoute :: Route
bestRoute = globalFasterAlternative routes

bestRouteWithJumps :: Route
bestRouteWithJumps = globalFasterAlternative'' altRoutes