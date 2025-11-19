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
   | typeIknotWrap tripCode = unKnot (makeIWrapChange tripCode)
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)
-- 17
makeIWrapChange :: [(Char, Char)] -> [(Char, Char)]
makeIWrapChange tripCode
   | listLength tripCode == 2 = []
   | otherwise = [head (tail tripCode)] ++ makeIWrapChange (tail tripCode)


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