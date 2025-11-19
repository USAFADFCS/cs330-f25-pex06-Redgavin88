-- pex6.hs 
-- unKnot Haskell
-- 3
-- name: Gavin Smith
-- 5
{- DOCUMENTATION:
-}
-- 8
listLength [] = 0
listLength a = 1 + listLength (tail a)
-- 11
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | typeIknot tripCode = unKnot (makeIChange tripCode) -- 15
   | typeIknotWrap tripCode = unKnot (removeWrap tripCode)
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)
-- 17
-- typeIIknot2 :: [(Char, Char)] -> Char a -> Char b -> Bool
typeIIknot2 tripCode
   | null tripCode = False
   | fst (head tripCode) == a && fst(head (drop 1 tripCode)) == b = True
   | fst (head tripCode) == b && fst(head (drop 1 tripCode)) == a = True
   | otherwise = typeIknot (tail tripCode)
makeIIChange :: [(Char, Char)] -> [(Char, Char)]
makeIIChange tripCode
   | null tripCode = []
   | (fst (head tripCode) /= (fst (head (drop 1 tripCode)))) && (snd (head tripCode) == (snd (head (drop 1 tripCode)))) && typeIIknot2 (drop 2 tripCode) (fst (head tripCode)) (fst (head (drop 1 tripCode))) = makeIIChange2 (drop 2 tripCode) (fst (head tripCode)) (fst (head (drop 1 tripCode)))
   | otherwise = [head tripCode] ++ (makeIIChange (tail tripCode))

-- makeIIChange2 :: [(Char, Char)] -> Char a -> Char b -> [(Char, Char)]
makeIIChange2 tripCode a b
   | null tripCode = []
   | fst (head tripCode) == a && fst(head (drop 1 tripCode)) == b = makeIIChange2 ( drop 2 tripCode)
   | fst (head tripCode) == b && fst(head (drop 1 tripCode)) == a = makeIIChange2 ( drop 2 tripCode)
   | otherwise = [head tripCode] ++ (makeIIChange2 (tail tripCode))

removeWrap :: [(Char, Char)] -> [(Char, Char)]
removeWrap tripCode
   | listLength tripCode == 2 = []
   | otherwise = [head (tail tripCode)] ++ removeWrap (tail tripCode)


typeIknotWrap :: [(Char, Char)] -> Bool
typeIknotWrap tripCode
   | null tripCode = False
   | listLength tripCode == 1 = False
   | fst(head tripCode) == fst(head (drop (listLength tripCode - 1) tripCode)) && snd(head tripCode) /= snd(head (drop (listLength tripCode - 1) tripCode)) = True
   | otherwise = False

typeIknot :: [(Char, Char)] -> Bool
-- typeIknot tripCode
typeIknot tripCode
   | null tripCode = False
   | listLength tripCode == 1 = False
   | fst (head tripCode) == fst (head (drop 1 tripCode)) && snd (head tripCode) /= snd (head (drop 1 tripCode)) = True -- 22
   | otherwise = typeIknot (tail tripCode)
-- 24
makeIChange :: [(Char, Char)] -> [(Char, Char)] 
makeIChange tripCode
   | null tripCode = []
   | listLength tripCode == 1 = tripCode
   | (fst (head tripCode) == fst (head (drop 1 tripCode))) && (snd (head tripCode) /= snd (head (drop 1 tripCode))) = makeIChange (drop 2 tripCode)
   | otherwise = [head tripCode] ++ (makeIChange (tail tripCode))

-- 29
main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)