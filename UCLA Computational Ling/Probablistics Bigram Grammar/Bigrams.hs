module Bigrams where

-----------------------------------------------------------------

data Numb = Z | S Numb deriving Show

-----------------------------------------------------------------
-- Introducing applyToAll/map

incrementAll :: [Numb] -> [Numb]
incrementAll [] = []
incrementAll (n:ns) = (S n) : (incrementAll ns)

pfAll :: [StrucDesc] -> [[String]]
pfAll [] = []
pfAll (x:xs) = (pf x) : (pfAll xs)

-- This function factors out the common pattern in incrementAll and pfAll.
-- (It's also predefined with the name 'map'.)
applyToAll :: (a -> b) -> [a] -> [b]
applyToAll f [] = []
applyToAll f (x:xs) = (f x) : (applyToAll f xs)

-- Now that we have applyToAll/map, we don't need the specific 
-- incrementAll and pfAll functions above: instead of
-- 'incrementAll xs' we can just write 'applyToAll (\n -> S n) xs', 
-- and instead of 'pfAll xs' we can just write 'applyToAll pf xs'.

-----------------------------------------------------------------
-- Data types for our bigram grammars

-- A rule says either ``you can step from word x to word y'', or 
-- ``you can end on word x''.
data GrammarRule = Step String String | End String deriving Show

-- A structural description is basically a non-empty list.
data StrucDesc = Last String | NonLast String StrucDesc deriving Show

-----------------------------------------------------------------
-- Sample grammars and structural descriptions

grammar1 :: [GrammarRule]
grammar1 = [Step "the" "hamsters",
            Step "the" "small",
            Step "the" "very",
            Step "very" "small",
            Step "small" "hamsters",
            Step "hamsters" "run",
            Step "hamsters" "walk",
            Step "run" "quickly",
            Step "walk" "quickly",
            Step "walk" "slowly",
            End "run",
            End "quickly",
            End "slowly"
            ]

grammar2 :: [GrammarRule]
grammar2 = grammar1 ++ [Step "very" "very"]

-- toy English phonotactics
grammar3 :: [GrammarRule]
grammar3 = [Step "b" "l",
            Step "l" "i",
            Step "n" "i",
            Step "z" "i",
            Step "i" "k",
            Step "i" "f",
            Step "i" "t",
            Step "f" "t",
            End "k",
            End "f",
            End "t"
            ]

sd1 :: StrucDesc
sd1 = NonLast "the" (NonLast "small" (NonLast "hamsters" (Last "run")))

sd2 :: StrucDesc
sd2 = NonLast "chipmunks" (Last "run")

sd3 :: StrucDesc
sd3 = NonLast "run" (Last "hamsters")

sd4 :: StrucDesc
sd4 = Last "quickly"

sd5 :: StrucDesc
sd5 = NonLast "very" (NonLast "very" (NonLast "small" (NonLast "hamsters" (Last "run"))))

-----------------------------------------------------------------

-- This tells us how a particular structural description is `pronounced'. 
-- (For these grammars, it's boring and simple.)
pf :: StrucDesc -> [String]
pf (Last s) = [s]
pf (NonLast s sd) = s:(pf sd)

-- Given a particular grammar, what are the words/symbols that are valid ending 
-- points according to that grammar?
enders :: [GrammarRule] -> [String]
enders [] = []
enders (r:rs) =
    case r of
    Step x y -> enders rs
    End x -> x : (enders rs)

-- Given a particular grammar and a particular word/symbol, what are the things 
-- that can follow that word/symbol according to that grammar?
successors :: [GrammarRule] -> String -> [String]
successors [] s = []
successors (r:rs) s =
    case r of
    Step x y -> if s == x then (y : successors rs s) else (successors rs s)
    End x -> successors rs s

-- Given a particular grammar and a particular structural description, 
-- is the structural description well-formed according to that grammar?
-- (NB: `elem x xs' checks whether x is in the list xs.)
wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Last s) = elem s (enders g)
wellFormed g (NonLast s sd) = (elem (firstWord sd) (successors g s)) && (wellFormed g sd)

firstWord :: StrucDesc -> String
firstWord (Last s) = s
firstWord (NonLast s sd) = s

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

sdMap :: (String -> String) -> StrucDesc -> StrucDesc
sdMap f (Last s) = Last (f s)
sdMap f (NonLast s sd') = NonLast (f s) sd'

lastWord :: StrucDesc -> String
lastWord sd = case sd of
	Last s -> s
	NonLast s sd' -> lastWord sd'

predecessors :: [GrammarRule] -> String -> [String]
predecessors [] s = []
predecessors (r:rs) s =
		case r of
			End x -> predecessors rs s
			Step x y -> if s == y then (x: predecessors rs s) else (predecessors rs s)

rulesFromSentence :: [String] -> [GrammarRule]
rulesFromSentence [] = []
rulesFromSentence (x:xs) = case xs of
		[] -> [End x]
		x':xs' -> Step x x' : rulesFromSentence xs

rulesFromText :: [[String]] -> [GrammarRule]
rulesFromText [[]] = []
rulesFromText xs =  concat (map rulesFromSentence xs)
 
extendByOne :: [GrammarRule] -> StrucDesc -> [StrucDesc]
extendByOne gram sd = map (\x -> NonLast x sd) (predecessors gram (firstWord sd))

extend :: [GrammarRule] -> Numb -> StrucDesc -> [StrucDesc]
extend gram num sd = case num of
		Z -> [sd]
		S num' -> sd : (concat (map (\sd' -> extend gram num' sd') (extendByOne gram sd)))

generate :: [GrammarRule] -> Numb -> [StrucDesc]
generate gram num = case num of
		Z -> []
		S num' -> concat( map (\x -> extend gram num' (Last x)) (enders gram))





-------------------------------------------------------------------------

--Discussion Function
type Position = [[String], String]

bigramloops :: [GrammarRule] -> String -> Bool
bigramloops g s = undefined

loophelper :: [GrammarRule] -> Position -> Bool
loophelper g (history, w) = 
    let nextwords = successors g w in 
    if any (\nextword -> elem nextword history) nextwords then True 
        else 
            let newpositions = map (\nextword -> (history ++ [w], nextword) nextwords in any (loophelper g) newpositions
 
---otherwise, create Position representing having moved to each nextWord and see if we can reach a loop from any of those
---the call to any will return false whenever some new positions is empty (ie whenver nextwords is empty)












































