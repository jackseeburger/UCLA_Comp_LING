module ExamProbCFG where

data Cat = S | NP | VP | N | D | V | PP | P | Adv deriving (Show,Eq,Ord)

data GrammarRule = End Cat String | Step Cat (Cat,Cat) deriving (Show,Eq)

data StrucDesc = Leaf Cat String | Binary Cat StrucDesc StrucDesc
                 deriving Show

type ProbCFG = ([(Cat,Double)],
                [((Cat,String),Double)],        -- terminal rules
                [((Cat,(Cat,Cat)),Double)],     -- nonterminal rules
                [Cat])

-- A minor variant of the grammar on page 384 of 
-- Manning and Schutze's "Foundations of Statistical NLP"
pcfg1 :: ProbCFG
pcfg1 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"dogs"), 0.1), ((NP,"telescopes"), 0.18), ((NP,"saw"), 0.04), ((NP,"cats"), 0.18), ((NP,"hamsters"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.7), ((VP,(VP,PP)), 0.3),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

-- Like above but reversed probabilities on the rules for expanding VP
pcfg2 :: ProbCFG
pcfg2 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"dogs"), 0.1), ((NP,"telescopes"), 0.18), ((NP,"saw"), 0.04), ((NP,"cats"), 0.18), ((NP,"hamsters"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.3), ((VP,(VP,PP)), 0.7),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

-- one more example grammar to test with
pcfg3 :: ProbCFG
pcfg3 = (   [(S,1.0)],
            [((NP,"NP"), 0.1), 
             ((D,"D"), 1.0), 
             ((N,"N"), 1.0), 
             ((P,"P"), 1.0), 
             ((V,"V"), 1.0), 
             ((VP,"VP"), 0.2)] ,
            [((S,(NP,VP)), 1.0),
             ((NP,(D,N)), 0.5), ((NP,(NP,PP)), 0.4), 
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.4), ((VP,(VP,PP)), 0.3), ((VP,(V,S)), 0.1)] ,
            [S,NP,D,N,VP,PP,P,V]
        )

--------------------------------------------------
-- Utility functions for getting information from grammars.

probLookup :: (Eq a) => [(a,Double)] -> a -> Double
probLookup []           key = 0.0
probLookup ((x,y):rest) key = if key == x then y else probLookup rest key

allCats :: ProbCFG -> [Cat]
allCats (starting,ending,transitions,cats) = cats

startProb :: ProbCFG -> Cat -> Double
startProb (starting,ending,transitions,cats) = probLookup starting

endProb :: ProbCFG -> Cat -> String -> Double
endProb (starting,ending,transitions,cats) c s = probLookup ending (c,s)

trProb :: ProbCFG -> Cat -> (Cat,Cat) -> Double
trProb (starting,ending,transitions,cats) c (c1,c2) = probLookup transitions (c,(c1,c2))

