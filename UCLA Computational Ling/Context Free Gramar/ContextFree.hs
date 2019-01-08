module ContextFree where

------------------------------------------------------------------------------------

data Cat = S | NP | VP | V | D | N | PP | P | IV | TV | RC deriving (Show,Eq)

data StrucDesc  = Binary Cat StrucDesc StrucDesc
                | Unary Cat StrucDesc
                | Leaf Cat String
                deriving Show

data GrammarRule    = BinaryStep Cat Cat Cat
                    | UnaryStep Cat Cat
                    | End Cat String
                    deriving Show

type Address = [Int]

grammar1 =  [   -- S rules
                BinaryStep S NP VP,
                -- VP rules
                BinaryStep VP V NP,
                BinaryStep VP V S,
                BinaryStep VP VP PP,
                UnaryStep  VP V,
                -- PP rules
                BinaryStep PP P NP,
                -- D rules
                End D "the",
                End D "a",
                -- N rules
                End N "cat",
                End N "dog",
                -- NP rules
                BinaryStep NP D N,
                End NP "John",
                End NP "Mary",
                -- V rules
                End V "left",
                End V "thinks",
                -- P rules
                End P "with"
            ]

grammar2 = [    BinaryStep S NP IV,
                BinaryStep NP N RC,
                UnaryStep NP N,
                BinaryStep RC NP TV,
                End N "dogs",   End N "cats",
                End IV "chase", End IV "sleep",
                End TV "chase"
            ]

sd1 = Binary S (Leaf NP "John") (Binary VP (Leaf V "left") (Leaf NP "Mary"))

sd2 = Binary S (Leaf NP "John")
               (Binary VP (Unary VP (Leaf V "left")) 
                          (Binary PP (Leaf P "with") (Binary NP (Leaf D "a") (Leaf N "cat")))
               )

sd3 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd1)

sd4 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd2)

sd5 = Binary S (Binary NP (Leaf N "dogs") (Binary RC (Unary NP (Leaf N "dogs")) (Leaf TV "chase"))) (Leaf IV "chase")

------------------------------------------------------------------------------------

pf :: StrucDesc -> [String]
pf (Binary c sd1 sd2) = pf sd1 ++ pf sd2
pf (Unary c sd) = pf sd
pf (Leaf c s) = [s]

leftmostLeaf :: StrucDesc -> String
leftmostLeaf (Leaf c s) = s
leftmostLeaf (Unary c sd) = leftmostLeaf sd
leftmostLeaf (Binary c sd1 sd2) = leftmostLeaf sd1

categoryOf :: StrucDesc -> Cat
categoryOf (Leaf c s) = c
categoryOf (Unary c sd) = c
categoryOf (Binary c sd1 sd2) = c

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Leaf c s) = elem c (enders g s)
wellFormed g (Unary c sd) = elem c (predecessorsUnary g (categoryOf sd)) && wellFormed g sd
wellFormed g (Binary c sd1 sd2) = elem c (predecessorsBinary g (categoryOf sd1, categoryOf sd2)) 
                                            && wellFormed g sd1 && wellFormed g sd2

depth :: StrucDesc -> Int
depth (Leaf c s) = 1
depth (Unary c sd) = 1 + depth sd
depth (Binary c sd1 sd2) = if (depth sd1 > depth sd2) then (1 + depth sd1) else (1 + depth sd2)

enders :: [GrammarRule] -> String -> [Cat]
enders [] x = []
enders (r:rs) x =
    case r of
    End c s -> if s == x then c : (enders rs x) else enders rs x
    UnaryStep c ch -> enders rs x
    BinaryStep c ch1 ch2 -> enders rs x

predecessorsUnary :: [GrammarRule] -> Cat -> [Cat]
predecessorsUnary [] x = []
predecessorsUnary (r:rs) x =
    case r of
    End c s -> predecessorsUnary rs x
    UnaryStep c ch -> if ch == x then (c : (predecessorsUnary rs x)) else (predecessorsUnary rs x)
    BinaryStep c ch1 ch2 -> predecessorsUnary rs x

predecessorsBinary :: [GrammarRule] -> (Cat,Cat) -> [Cat]
predecessorsBinary [] x = []
predecessorsBinary (r:rs) x =
    case r of
    End c s -> predecessorsBinary rs x
    UnaryStep c ch -> predecessorsBinary rs x
    BinaryStep c ch1 ch2 -> if (ch1,ch2) == x then (c : (predecessorsBinary rs x)) else (predecessorsBinary rs x)

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------
numUnaries :: StrucDesc -> Int
numUnaries (Leaf c s) = 0
numUnaries (Unary c sd) = 1 + numUnaries sd
numUnaries (Binary c sd1 sd2) = numUnaries sd1 + numUnaries sd2

collectNPs :: StrucDesc -> [StrucDesc]
collectNPs sd = (if categoryOf sd == NP then [sd] else [])
                ++
                case sd of
                    Leaf c s -> []
                    Unary c sd -> collectNPs sd
                    Binary c sd1 sd2 -> collectNPs sd1 ++ collectNPs sd2

--getSubtree :: StrucDesc -> Address -> StrucDesc 
--getSubtree sd [] = sd
--getSubtree (Leaf c s) (n:ns) = if n == 0 then 








brackets :: StrucDesc -> String
brackets (Binary c sd1 sd2) = "[" ++ brackets sd1 ++ brackets sd2 ++ "]"
brackets (Unary c sd) =  "[" ++ brackets sd ++ "]"
brackets (Leaf c s) = s

labeledBrackets :: StrucDesc -> String
labeledBrackets (Binary c sd1 sd2) = "[" ++ (show c) ++  labeledBrackets sd1 ++ labeledBrackets sd2 ++ "]"
labeledBrackets (Unary c sd) =  "[" ++ show c ++ brackets sd ++ "]"
labeledBrackets (Leaf c s) =  s

numNPs :: StrucDesc -> Int
numNPs (Binary c sd1 sd2) = if c == NP then 1 + numNPs sd1 + numNPs sd2 else numNPs sd1 + numNPs sd2
numNPs (Unary c sd) = if c == NP then 1 + numNPs sd  else numNPs sd
numNPs (Leaf c s) = if c == NP then 1 else 0

--numViolations :: [GrammarRule] -> StrucDesc -> Int

--numViolations g (Unary c sd) = if (elem (UnaryStep c (categoryOf sd))) then 0 + (numViolations g sd) else 1 + (numViolations g sd)
--numViolations g (Binary c sd1 sd2) = if elem (UnaryStep c (categoryOf sd1)) == False && elem (UnaryStep c (categoryOf sd2))
--        then (1 + (numViolations g sd1) + (numViolations g sd2)) 
--        else 
--            (if elem (UnaryStep c (categoryOf sd2)) == False && elem (UnaryStep c (categoryOf sd1))
--         then (1 + (numViolations g sd1) + (numViolations g sd2)) 
--         else 
--             (if elem (UnaryStep c (categoryOf sd2)) == False && elem (UnaryStep c (categoryOf sd1)) == False then 2 + (numViolations g sd1) + (numViolations g sd2) else 0 + (numViolations g sd1) + (numViolations g sd2)))
numViolations :: [GrammarRule] -> StrucDesc -> Int
numViolations g (Leaf c s) = if elem c (enders g s) then 0 else 1
numViolations g (Unary c sd) = if elem c (predecessorsUnary g (categoryOf sd))  then numViolations g sd else 1 + numViolations g sd
numViolations g (Binary c sd1 sd2) = if elem c (predecessorsBinary g (categoryOf sd1, categoryOf sd2)) then numViolations g sd1 + numViolations g sd2 else 1 + numViolations g sd1 + numViolations g sd2


sdMap :: (String -> String) -> StrucDesc -> StrucDesc
sdMap f (Binary c sd1 sd2) = Binary c (sdMap f sd1) (sdMap f sd2)
sdMap f (Unary c sd) = Unary c (sdMap f sd)
sdMap f (Leaf c s) = Leaf c (f s) 

longestPath :: StrucDesc -> [Cat]
longestPath (Leaf c s) = [c]
longestPath (Unary c sd) = c : longestPath sd
longestPath (Binary c sd1 sd2) = if (length (longestPath sd1)) >= (length (longestPath sd2)) then c : longestPath sd1 else c : longestPath sd2

allPaths :: StrucDesc -> [[Cat]]
allPaths (Leaf c s) = [[c]]
allPaths (Unary c sd) = map (\paths -> c:paths) (allPaths sd)
allPaths (Binary c sd1 sd2) = (map (\paths1 -> c:paths1) (allPaths sd1)) ++ (map (\paths2 -> c:paths2) (allPaths sd2))

addressesOfNPs :: StrucDesc -> [Address]
addressesOfNPs (Leaf c s) = if c == NP then [[]] else []
addressesOfNPs (Unary c sd) = let help = map (\temp -> [0] ++ temp) (addressesOfNPs sd) in if c == NP then [[]] ++ help else help
addressesOfNPs (Binary c sd1 sd2) = let help = map (\temp -> [0] ++ temp) (addressesOfNPs sd1) ++ map (\temp -> [1] ++ temp) (addressesOfNPs sd2) in if c == NP then [[]] ++ help else help

ccommand :: Address -> Address -> Bool
ccommand = undefined

replace :: StrucDesc -> Address -> StrucDesc -> StrucDesc
replace sd [] r = r
replace (Leaf c s) (w:ws) r = Leaf c s
replace (Unary c sd) (w:ws) r = if w == 0 then Unary c (replace sd ws r) else Unary c sd
replace (Binary c sd1 sd2) (w:ws) r =  case w of 
                                             0 -> Binary c (replace sd1 ws r) sd2
                                             1 -> Binary c sd1 (replace sd2 ws r)
                                             _ -> Binary c sd1 sd2



