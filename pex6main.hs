-- pex6.hs 
-- unKnot Haskell
-- 3
-- name: Gavin Smith
-- 5
{- DOCUMENTATION: None
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
   | typeIIknot tripCode = unKnot (makeIIChange tripCode)
   | typeIIknotWrap tripCode = unKnot (makeIIChange2 (removeWrap (tripCode)) (fst (head tripCode)) (fst (head (drop (listLength tripCode - 1) tripCode))))
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)
-- 17
typeIIknotWrap :: [(Char, Char)] -> Bool
typeIIknotWrap tripCode
   | null tripCode = False
   | listLength tripCode < 3 = False
   | snd (head tripCode) == (snd (head (drop (listLength tripCode - 1) tripCode))) && typeIIknot2 (removeWrap tripCode) (fst (head tripCode)) (fst (head (drop (listLength tripCode - 1) tripCode))) = True
   | otherwise = False


typeIIknot :: [(Char, Char)] -> Bool
typeIIknot tripCode
   | null  tripCode = False
   | listLength tripCode < 3 = False
   | snd (head tripCode) == (snd (head (drop 1 tripCode))) && typeIIknot2 (drop 2 tripCode) (fst (head tripCode)) (fst (head (drop 1 tripCode))) = True
   | otherwise = typeIIknot (tail tripCode)

typeIIknot2 :: [(Char, Char)] -> Char -> Char -> Bool
typeIIknot2 tripCode a b
   | null tripCode = False
   | listLength tripCode < 2 = False   
   | fst (head tripCode) == a && fst(head (drop 1 tripCode)) == b = True
   | fst (head tripCode) == b && fst(head (drop 1 tripCode)) == a = True
   | otherwise = typeIIknot2 (tail tripCode) a b

makeIIChange :: [(Char, Char)] -> [(Char, Char)]
makeIIChange tripCode
   | null tripCode = []
   | listLength tripCode < 3 = tripCode 
   | snd (head tripCode) == (snd (head (drop 1 tripCode))) && typeIIknot2 (drop 2 tripCode) (fst (head tripCode)) (fst (head (drop 1 tripCode))) = makeIIChange2 (drop 2 tripCode) (fst (head tripCode)) (fst (head (drop 1 tripCode)))
   | otherwise = [head tripCode] ++ (makeIIChange (tail tripCode))

makeIIChange2 :: [(Char, Char)] -> Char -> Char -> [(Char, Char)]
makeIIChange2 tripCode a b
   | null tripCode = []
   | listLength tripCode < 3 = [] 
   | fst (head tripCode) == a && fst(head (drop 1 tripCode)) == b = drop 2 tripCode
   | fst (head tripCode) == b && fst(head (drop 1 tripCode)) == a = drop 2 tripCode
   | otherwise = [head tripCode] ++ (makeIIChange2 (tail tripCode) a b)

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

   let t02 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("   test case t02 - tripcode: " ) -- "Unkot"
   print(t02)
   print("   result:" ++ unKnot t02) -- "Unkot"

   let t03 = [('a','u'),('b','u'),('a','o'),('b','o')]
   print("   test case t03 - tripcode: " )
   print(t03)
   print("   result:" ++ unKnot t03) -- "Unkot"

   let t04 = [('a','o'),('b','u'),('a','u'),('b','o')]
   print("   test case t04 - tripcode: " )
   print(t04)
   print("   result:" ++ unKnot t04) -- "Unkot"

   let t05 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t05 - tripcode: " )
   print(t05)
   print("   result:" ++ unKnot t05)  

   let t06 = [('a','o'),('q','u'), ('a','u')]
   print("   test case t06 - tripcode: " )
   print(t06)
   print("   result:" ++ unKnot t06) -- [('q','u')]

   let t07 = [('a','o'),('a','u'), ('q','u')]
   print("   test case t07 - tripcode: " )
   print(t07)
   print("   result:" ++ unKnot t07) -- [('q','u')]

   let t08 = [('a','o'),('b','o'), ('a','u'), ('b','u'),('q','u')]
   print("   test case t08 - tripcode: " )
   print(t08)
   print("   result:" ++ unKnot t08) -- [('q','u')]

   let t09 = [('a','u'),('b','o'), ('a','o'), ('b','u'),('q','u')]
   print("   test case t09 - tripcode: " )
   print(t09)
   print("   result:" ++ unKnot t09) -- [('a','o'),('b','o'), ('a','u'), ('b','u') ('q','u')]

   let t10 = [('a','u'),('b','o'), ('a','o'), ('b','u'),('q','u'),('c','o'),('c','u')]
   print("   test case t10 - tripcode: " )
   print(t10)
   print("   result:" ++ unKnot t10) -- [('a','o'),('b','o'), ('a','u'), ('b','u') ('q','u')]

   let t11 = [('a','u'),('b','o'), ('a','o'), ('q','u'),('b','u'),('c','o'),('c','u')]
   print("   test case t11 - tripcode: " )
   print(t11)
   print("   result:" ++ unKnot t11) -- [('q','u')]

   let t12 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('q','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t12 - tripcode: " )
   print(t12)
   print("   result:" ++ unKnot t12) -- [('q','u')]

   