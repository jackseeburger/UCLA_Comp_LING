module ProbFSA where

type State = Int

type ProbFSA = ([(State,Double)],                   -- start distribution
                [(State,Double)],                   -- ending probabilities
                [((State,State),Double)],           -- transition probabilities
                [(((State,State),String),Double)],  -- emissions
                [State])                            -- all states

data StrucDesc = End State | Step State String StrucDesc deriving Show

-- Example Grammars
pfsa1 :: ProbFSA
pfsa1 = (   -- start distribution
            [(1,1.0)] ,
            -- end probabilities
            [(5,0.5)] ,
            -- transition probabilities
            [((1,2), 0.3), ((1,3), 0.7),
             ((2,3), 1.0),
             ((3,4), 1.0),
             ((4,4), 0.4),
             ((4,5), 0.6),
             ((5,1), 0.5)] ,
            -- emission probabilities
            [(((1,2),"these"), 0.5), (((1,2),"some"), 0.5),
             (((1,3),"they"), 0.4), (((1,3),"these"), 0.6),
             (((2,3),"dogs"), 0.3), (((2,3),"buffalo"), 0.7),
             (((3,4),"buffalo"), 0.6), (((3,4),"damaged"), 0.4),
             (((4,4),"damaged"), 0.7), (((4,4),"nice"), 0.3),
             (((4,5),"unicorns"), 0.8), (((4,5),"stuff"), 0.2),
             (((5,1),"and"),0.1)],
            [1,2,3,4,5]
        )

pfsa2 :: ProbFSA
pfsa2 = (   -- start distribution
            [(100,1.0)] ,
            -- end probabilities
            [(400,0.5)] ,
            -- transition probabilities
            [((100,200), 0.4), ((100,300), 0.6),
             ((200,400), 1.0),
             ((300,200), 0.3), ((300,400), 0.7),
             ((400,300), 0.5)] ,
            -- emission probabilities
            [(((100,200),"c"), 1.0), 
             (((100,300),"a"), 0.7), (((100,300),"b"), 0.3), 
             (((200,400),"a"), 0.2), (((200,400),"d"), 0.8), 
             (((300,200),"c"), 0.6), (((300,200),"d"), 0.4), 
             (((300,400),"b"), 0.5), (((300,400),"c"), 0.5), 
             (((400,300),"b"), 0.3), (((400,300),"d"), 0.7)] ,
            [100,200,300,400]
        )


pfsa3 :: ProbFSA
pfsa3 = (   -- start distribution
            [(10,1.0)] ,
            -- end probabilities
            [(40,0.5)] ,
            -- transition probabilities
            [((10,20), 0.4), ((10,30), 0.6),
             ((20,40), 1.0),
             ((30,40), 1.0),
             ((40,10), 0.5)] ,
            -- emission probabilities
            [(((10,20),"a"), 0.2), (((10,20),"b"), 0.8), 
             (((10,30),"a"), 0.7), (((10,30),"c"), 0.3), 
             (((20,40),"c"), 0.6), (((20,40),"d"), 0.4), 
             (((30,40),"b"), 0.1), (((30,40),"d"), 0.9), 
             (((40,10),"e"), 1.0)] ,
            [10,20,30,40]
        )

--------------------------------------------------
-- Utility functions for getting information from grammars.

probLookup :: (Eq a) => [(a,Double)] -> a -> Double
probLookup [] key = 0.0
probLookup ((x,y):rest) key = if key == x then y else probLookup rest key

allStates :: ProbFSA -> [State]
allStates (starting,ending,transitions,emissions,states) = states

startProb :: ProbFSA -> State -> Double
startProb (starting,ending,transitions,emissions,states) = probLookup starting

endProb :: ProbFSA -> State -> Double
endProb (starting,ending,transitions,emissions,states) = probLookup ending

trProb :: ProbFSA -> State -> State -> Double
trProb (starting,ending,transitions,emissions,states) st1 st2 = probLookup transitions (st1,st2)

emProb :: ProbFSA -> (State,State) -> String -> Double
emProb (starting,ending,transitions,emissions,states) (st1,st2) str = probLookup emissions ((st1,st2),str)

--------------------------------------------------


probForward :: ProbFSA -> [String] -> State -> Double
probForward pfsa output st =
    if output == [] then
        startProb pfsa st
    else
        let (ws,w) = (init output, last output) in
        sum (map (\prev -> probForward pfsa ws prev * trProb pfsa prev st * emProb pfsa (prev,st) w) (allStates pfsa))



probBackward :: ProbFSA -> [String] -> State -> Double
probBackward pfsa [] st = endProb pfsa st
probBackward pfsa (w:ws) state = sum ( map (\nextst -> trProb pfsa state nextst * emProb pfsa (state, nextst) w * probBackward pfsa ws nextst) (allStates pfsa))

viterbiProbForward :: ProbFSA -> [String] -> State -> Double
viterbiProbForward pfsa [] state = startProb pfsa state
viterbiProbForward pfsa output state = let (ws, w) = (init output, last output) in
                                            maximum (map (\previousSt -> viterbiProbForward pfsa ws previousSt * trProb pfsa previousSt state * emProb pfsa (previousSt, state) w) (allStates pfsa))

--helper for pair
tuplehelp :: [(a, Double)] -> (a, Double)
tuplehelp tuplelist = case tuplelist of 
                        [] -> (undefined, 0)
                        (x:xs) -> if snd x > snd(tuplehelp xs) then x else tuplehelp xs

                 

viterbiPairForward :: ProbFSA -> [String] -> State -> (State,Double)
viterbiPairForward pfsa [] state = (undefined, startProb pfsa state)
viterbiPairForward pfsa output state = let (ws, w) = (init output, last output) in
    tuplehelp (map (\prev->(prev, (( snd(viterbiPairForward pfsa ws prev)) * trProb pfsa prev state * emProb pfsa (prev, state) w))) (allStates pfsa))

viterbiProbBackward :: ProbFSA -> [String] -> State -> Double
viterbiProbBackward pfsa [] state = endProb pfsa state
viterbiProbBackward pfsa (w:ws) state = 
                         maximum (map (\nextSt ->  trProb pfsa state nextSt * emProb pfsa (state, nextSt) w * viterbiProbBackward pfsa ws nextSt) (allStates pfsa))

--redo helper to take care of undefined case
tuplehelp2 :: [(StrucDesc, Double)] -> (StrucDesc, Double)
tuplehelp2 tuplelist = case tuplelist of 
                        [] -> ((End 999) , 0)
                        (x:xs) -> if snd x >= snd(tuplehelp2 xs) then x else tuplehelp2 xs

viterbiStrucDesc :: ProbFSA -> [String] -> State -> (StrucDesc,Double)
viterbiStrucDesc pfsa [] st = ((End st), 0)
viterbiStrucDesc pfsa (w:ws) st = 
                tuplehelp2 (map(\nextst->((Step st w  (fst (viterbiStrucDesc pfsa ws nextst))), trProb pfsa st nextst * emProb pfsa (st, nextst) w * viterbiProbBackward pfsa ws nextst)) (allStates pfsa))
