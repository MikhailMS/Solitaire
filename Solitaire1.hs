{-
   Solitaire1.hs
   Assignment 2 
   Updated by Mikhail Molotkov, BEng Software Engineering second year student.
   Updated : 17/12/15
-}
module Solitaire1 where
 import System.Random
 import Data.List
 import Data.Maybe
 import Debug.Trace

-- playing card data structures
 data Suit = Hearts|Clubs|Diamonds|Spades
             deriving (Eq, Show,Enum) 
 allSuits = enumFrom Hearts
              
 data Pip = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
            deriving (Eq,Ord,Show,Enum) 
 allPips = enumFrom Ace
                                   
 type Card = (Pip,Suit) 
 type Deck = [Card]
 
 ------------------------------------------------------------------  
-- 8 off solitaire data structures

 type EOBoard = (Foundations, Columns, Reserves)

 type Foundations = [Card] -- only need to know top card
 
 type Columns = [[Card]]
 
 type Reserves = [Card]
 -------------------------------------------------------------------
 
-- next card in suit 
 sCard :: Card->Card
 sCard (p,s) = ((succ p),s)
 
-- predecessor in suit 
 pCard :: Card->Card
 pCard (p,s) =((pred p),s)

-- build pack with comprehensions 
 pack :: Deck
 pack =  [(p,s)| p<- allPips,
                 s<- allSuits ]      
         
-- shuffle the pack
 shuffle :: Int -> [Card]
 shuffle seed =  
  let
    gen= mkStdGen seed
    weights = take 52 (randoms gen :: [Int])
    dset = (map fst (sortBy  
               (\ (_,w1)(_,w2)  -> (compare w1 w2)) 
               (zip pack weights)))
  in
   dset           

-----------------------------------------------------------------
  
-- 8 off deal, given seed
 eODeal :: Int-> EOBoard 
 eODeal seed = 
  let
   spack = shuffle seed
   cols  = [(take 6 spack),          (take 6 (drop 6 spack)),  (take 6 (drop 12 spack)), (take 6 (drop 18 spack)), 
           (take 6 (drop 24 spack)), (take 6 (drop 30 spack)), (take 6 (drop 36 spack)), (take 6 (drop 42 spack))]
   fnds  = []
   res   = [spack!!48,spack!!49,spack!!50,spack!!51]
  in
   (fnds,cols,res)          

-- example
 b1@(f1,c1,r1) = eODeal 21
 b2@(f2,c2,r2) = toFoundations b1
 
-----------------------------------------------------------------
-- isAce, isKing utilities
 isAce :: Card-> Bool
 isAce (p,_) = (p==Ace)
 
 isKing :: Card-> Bool
 isKing (p,_) = (p==King)
----------------------------------------------------------------
-- move everthing that will go to foundations
 toFoundations :: EOBoard -> EOBoard 
 toFoundations bd = 
  toFA (reserveAcesToFoundations bd) -- because reserveAcesToFoundations is called only once
 
 -- helper function (toFoundations) 
 toFA :: EOBoard -> EOBoard -- called recursively till no change 
 toFA bd
  | bd/=cafBd = toFA cafBd
  | bd/=rtfBd = toFA rtfBd
  | bd/=chfBd = toFA chfBd
  |otherwise = bd
  where
   cafBd=colAcesToFoundations bd
   rtfBd=reserveToFoundations bd
   chfBd=colHeadsToFoundations bd
     
------------------------------------------------------------
-- reserveAcesToFoundations
-- move all aces from reserve to foundations
-- can only happen at start of game, first thing that happens,foundations must be []
 reserveAcesToFoundations :: EOBoard -> EOBoard
 reserveAcesToFoundations (fnds,cols,res)
  |(null fnds) = ((filter isAce res),cols,(filter (not.isAce) res))
  |otherwise = (fnds,cols,res)
 
------------------------------------------------------------
--reserveToFoundations
-- non-Aces from reserve to foundations
 reserveToFoundations :: EOBoard -> EOBoard 
 reserveToFoundations (fnds,cols,res) = 
      ((map (\f-> (if (not (isKing f))&&(elem (sCard f) res) then (sCard f) else f)) fnds), -- update fns
       cols,
       (filter (\r-> (not(elem (pCard r) fnds )))res))  --update res
       
------------------------------------------------------------
-- column aces to foundations    
 colAcesToFoundations :: EOBoard -> EOBoard
 colAcesToFoundations (fnds,cols,res) = 
  (fnds ++ (filter isAce colHeads),
  (filter (not.null) (map (\col -> if (isAce (head col)) then (tail col) else col) cols)),
   res)
   where
    colHeads = map head cols
------------------------------------------------------------
-- column heads to foundations
 colHeadsToFoundations :: EOBoard -> EOBoard 
 colHeadsToFoundations (fnds,cols,res) = 
  ((map (\f-> if (not(isKing f))&&(elem (sCard f) colHeads) then (sCard f) else f) fnds), -- **f can't be king update foundations
   (filter (not.null) -- update columns
           (map (\col@(hc:rc)-> if (elem (pCard hc) fnds) then rc else col)cols)), -- may have emptied a column
   res) 
  where
   colHeads = map head cols 
------------------------------------------------------------
-- column heads to reserve
 colHeadsToReserve :: EOBoard -> EOBoard
 colHeadsToReserve (fnds,cols,res)
   |(length res < 8) = (fnds, 
                        (filter (not.null) -- update columns
                           (map (\col@(hc:rc)-> if (col /= [])&&(hc == c) then rc else col)(filter (\x -> x/=[]) cols))) ++ (replicate (length $ filter (\x -> length x == 0) cols) []) ,
                        (if (elem c colHeads) then c:res else res) )
   |otherwise = (fnds,cols,res)
   where
        colHeads = map head cols
        c = head colHeads
------------------------------------------------------------
-- reserve to column head 
 reserveToCol :: EOBoard -> EOBoard
 reserveToCol (fnds,cols,res)
   |(not (isKing c)) = (fnds,
                        (filter (not.null) -- update columns
                          (map (\col -> if ((col /= [])&&((head col) == (sCard c))) then (c:col) else col)(filter (\x -> x/=[]) cols)))
                            ++ (replicate (length $ filter (\x -> length x == 0) cols) []),
                        (filter (\f -> (f /= c)) res) )
        
   |(isKing c)&&(elem [] cols) = (fnds,
                    (filter (not.null) -- update columns
                      (map (\col -> if (col == []) then c:col else (if head col == c then (tail col) else col))cols)),
                    (filter (\f -> (f /= c)) res) )
   |otherwise = (fnds,cols,res)
   where 
        colHeads = map head (filter (\x -> x/=[]) cols)
        c = head res
------------------------------------------------------------
-- between column's heads 
 colHeadsToCol :: EOBoard -> EOBoard  
 colHeadsToCol (fnds,cols,res)
   |(not (isKing c))&&(elem (sCard c) colHeads) = (fnds,
                                                   (filter (not.null) -- update columns
                                                     (map (\col@(hc:rc) -> if ((hc == c)&&(col /= [])) 
                                                                            then (tail col) else (if hc == (sCard c) then (c:col)  
                                                                                   else col))(filter (\x -> x/=[]) cols)))++ (replicate (length $ filter (\x -> length x == 0) cols) []),
                                                   res)
   |(isKing c)&&(elem [] cols) = (fnds,
                                  (filter (not.null) -- update columns
                                    (map (\col -> if (col == []) then c:col else (if head col == c then (tail col) else col))cols)),
                                  res)
   |otherwise = (fnds,cols,res)
   where 
        colHeads = map head (filter (\x -> x/=[]) cols)
        c = head colHeads
------------------------------------------------------------