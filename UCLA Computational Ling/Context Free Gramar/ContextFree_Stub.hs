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

getSubtree :: StrucDesc -> Address -> StrucDesc 
getSubtree sd [] = sd
getSubtree (Leaf c s) (n:ns) = if n == 0 then 







brackets :: StrucDesc -> String
brackets = undefined

labeledBrackets :: StrucDesc -> String
labeledBrackets = undefined

numNPs :: StrucDesc -> Int
numNPs = undefined

numViolations :: [GrammarRule] -> StrucDesc -> Int
numViolations = undefined

sdMap :: (String -> String) -> StrucDesc -> StrucDesc
sdMap = undefined

longestPath :: StrucDesc -> [Cat]
longestPath = undefined

allPaths :: StrucDesc -> [[Cat]]
allPaths = undefined

addressesOfNPs :: StrucDesc -> [Address]
addressesOfNPs = undefined

ccommand :: Address -> Address -> Bool
ccommand = undefined

replace :: StrucDesc -> Address -> StrucDesc -> StrucDesc
replace = undefined

move :: Address -> StrucDesc -> StrucDesc
move = undefined

