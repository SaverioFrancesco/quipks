
{-# LANGUAGE FlexibleContexts      #-}
module Graph where

import Data.Array as Array
import Data.List

import EntangleMonad
import Examples
import BitQubitId

import Data.Array
import Data.Monoid

import Control.Monad.Writer  
import qualified Data.Graph
import Complex
import           Data.Matrix hiding (identity, (!), matrix, zero, (<->), (<|>))

import           QMatrixQuipks
import Dtmc

{-Dot file compiler, by Luca Foschiani-}

type Vertex = Int
type Table a = Array Vertex [Edge]
type Graph = Table [(Edge, Vertex)]
            
-- Needed for the buildG function (the second element of the tuple is the number of vertices)
type Bounds  = (Vertex, Vertex)

-- Edges data type
data Edge
        = GateEdge Vertex (String,[QubitId],[QubitId]) Vertex       -- Edge representing a gate
        | MeasureEdge Vertex (QubitId,BitId) Vertex                 -- Edge representing a Measurement on one qubit
        | LoopEdge Vertex String                                    -- Edge used to represent recursive circuits
        deriving (Show)
-- A Labeling contains the information about bits in a certain state (vertex)
type Labeling a = Vertex -> [(BitId,Int)]
    
-- Labelled Graph
data LabGraph n e = LabGraph (Graph) (Labeling n)
-- Returns a list of vertices
vertices :: LabGraph t t1 -> [Vertex] 
vertices (LabGraph gr _) = indices gr

-- Returns all the vertices' labels of the graph
labels :: LabGraph t t1 -> [[(BitId,Int)]]
labels (LabGraph gr l) = map l (indices gr)

-- Returns a list of edges
edges :: Graph -> [Edge]
edges g = [(GateEdge v1 l v2) | v1 <- indices g, (GateEdge v1 l v2) <- g!v1] ++
          [(MeasureEdge v1 c v2) | v1 <- indices g, (MeasureEdge v1 c v2) <- g!v1] ++
          [(LoopEdge v s) | v <- indices g, (LoopEdge v s) <- g!v]


-- Generates a labGraph from a tree representing the quantum circuit
labGraphFromTree :: CircTree RecAction -> LabGraph n e
labGraphFromTree tree = LabGraph graph labels
    where
        graph = buildG bounds edges
        labels = getNodeLabels tree $ initLabels tree
        bounds = (1,numOfGates tree)
        edges = extractGates tree 1

-- Builds a (non labelled) graph from a list of edges
buildG :: Bounds -> [Edge] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds ([(v1, (GateEdge v1 l v2)) | (GateEdge v1 l v2) <- edges] ++
                                                       [(v1, (MeasureEdge v1 c v2)) | (MeasureEdge v1 c v2) <- edges] ++
                                                       [(v, (LoopEdge v s)) | (LoopEdge v s) <- edges])

-- Computes the labels needed to build the graph
getNodeLabels :: (Num a, Num t, Ord a) => CircTree t1 -> [(BitId, t)] -> a -> [(BitId, t)]
getNodeLabels (GateNode _ _ _ t) s n
        | n>1       = getNodeLabels t s (n-1)
        | otherwise = s

getNodeLabels (MeasureNode _ bit t1 t2) s n
        | n>1       = if n <= numOfGates t1 + 1
                        then getNodeLabels t1 s (n-1)
                        else getNodeLabels t2 ((bit,1):(filter ((/=bit).fst) s)) (n - (numOfGates t1) - 1)
        | otherwise = s

getNodeLabels (LeafNode _ ) s _ = s

-- Initially set the value of all bits to be 0
initLabels :: Num t1 => CircTree t -> [(BitId, t1)]
initLabels (GateNode _ _ _ t) = initLabels t
initLabels (MeasureNode qubit bit t1 t2) = (bit,0):(initLabels t1)
initLabels (LeafNode _) = []


-- Calculates the number of gates required to represent the (sub)tree rooted in t
numOfGates :: Num a => CircTree t -> a
numOfGates (GateNode _ _ _ t) = numOfGates t + 1
numOfGates (MeasureNode _ _ t1 t2) = numOfGates t1 + numOfGates t2 + 1
numOfGates (LeafNode _) = 1


-- Computes the edges required to represent the whole circuit
extractGates :: CircTree RecAction -> Vertex -> [Edge]
extractGates (GateNode gate qubits1 qubits2 t) n = (GateEdge n (gate,qubits1,qubits2) (n+1)):extractGates t (n+1)
extractGates (MeasureNode qubit bit t1 t2) n = (MeasureEdge n (qubit,bit) (n+1)):
                                               (MeasureEdge n (qubit,bit) (n + numOfGates t1 + 1)):
                                               (extractGates t1 (n+1)) ++ (extractGates t2 (n + (numOfGates t1) + 1))
extractGates (LeafNode Loop) n = [(LoopEdge n "Loop")]          -- needed for recursive programs
extractGates (LeafNode _) _ = []



-- Outputs a graph in .dot format
showGraphViz :: LabGraph t t1 -> String
showGraphViz (LabGraph gr lab)  = 
    "digraph {\n" ++
    --"rankdir=LR;\n" ++                                    -- uncomment -> render graph from left to right
    (concatMap showNode $ indices gr) ++
    (concatMap showEdge $ edges gr) ++
    "}\n"
    where
        showEdge (GateEdge from t to) = show from ++ " -> " ++ show to ++ " [label = \"" ++ showTripleGate t ++ "\"];\n"
        showEdge (MeasureEdge from t to) = show from ++ " -> " ++ show to ++ " [label = \"" ++ showTripleMeasure t ++ "\"];\n"
        showEdge (LoopEdge from t) = show from ++ " -> " ++ "1" ++ " [label = \"" ++ t ++ "\"];\n"
        showNode v = show v ++ " [label = "  ++  (show $ intercalate ", " $ map showPair $ sort $ lab v) ++ "];\n"
        showTripleGate (gate,qubits1,qubits2) = "Gate '" ++ gate ++ "' on " ++ showQubitList qubits1 ++
                                                (if length qubits2 > 0 then " controls " else "") ++ showQubitList qubits2
        showTripleMeasure (qubit,bit) = "Measure " ++ show qubit ++ " -> " ++ show bit
        showPair (bit,int) = show bit ++ "=" ++ show int
        showQubitList l = intercalate "," $ map show l


showGraphV :: LabGraph t t1 -> String
showGraphV (LabGraph gr lab) = 
    "digraph {\n" ++
    --"rankdir=LR;\n" ++       -- uncomment -> render graph from left to right
    (concatMap showNode $ indices gr) ++
    (concatMap showEdge $ edges gr) ++
    "}\n"
    where
        showEdge (GateEdge from t to) = "GateEdge " ++ show from ++"," ++ show to ++ "," ++ showTripleGate t ++ ";\n"
        showEdge (MeasureEdge from t to) ="MeasureEdge " ++ show from ++ "," ++ show to ++ "," ++ showTripleMeasure t ++ ";\n"
        showEdge (LoopEdge from t) = "LoopEdge "++ show from ++ "," ++ "1" ++ "," ++ t ++ ";\n"
        showNode v = show v ++ " ," ++  (show $ intercalate ", " $ map showPair $ sort $ lab v) ++ ";\n"


        showTripleGate (gate,qubits1,qubits2) = "Gate '" ++ gate ++ "' on " ++ showQubitList qubits1 ++
                                                (if length qubits2 > 0 then " controls " else "") ++ showQubitList qubits2
        showTripleMeasure (qubit,bit) = "Meaxsure " ++ show qubit ++ " -> " ++ show bit
        showPair (bit,int) = show bit ++ "=" ++ show int
        showQubitList l = intercalate "," $ map show l


-----------------------------------------------------------------------
{-
________        .__        __            
\_____  \  __ __|__|_____ |  | __  ______
 /  / \  \|  |  \  \____ \|  |/ / /  ___/
/   \_/.  \  |  /  |  |_> >    <  \___ \ 
\_____\ \_/____/|__|   __/|__|_ \/____  >
       \__>        |__|        \/     \/                                                                                                                                                                      
-}  
------------------------------------------------------------------------
{-
Author: Francesco Saverio Comisso
Mail: mitramdhir[at]gamil.com

Core code of the tranlator.
-}

debugMode=False


data ActionKind = Shure | ZeroMesure | OneMesure

traverseArc:: Edge -> LabGraph t t1 -> Int -> (String, Int, Matrix (Complex Double), ActionKind)

traverseArc  (GateEdge a (name, qubits1@(x:qx),  qubits2) b) g1 countqbits =
             if length qubits2 > 0 
                then 
                if length qubits1 ==1 
                then (name,b,  cGateMulti  [(unQubitId x)] (map (\x->unQubitId x) qubits2) (nameToMatrix name) countqbits , Shure) 
                else (name,b,  cGateMulti (map (\x->unQubitId x) qubits1) (map (\x->unQubitId x) qubits2) (nameToMatrix name) countqbits, Shure) 
                else
                if length qubits1 ==1 
                then (name, b, unaryGateAt (unQubitId x) (nameToMatrix name) countqbits {- identityOnNQB countqbits -} , Shure) --nameToMatrix name
                else (name,b, gateMulti (map (\x->unQubitId x) qubits1) (nameToMatrix name) countqbits , Shure)
                                        
traverseArc  (MeasureEdge a info_mesure@(qbit_id, bit_id) b) g1 countqbits = 
            if (valueOfBitInState g1 bit_id b)==0 
                then ("mesure UL ",b, unaryGateAt (unQubitId qbit_id) (measure UL) countqbits, ZeroMesure) 
                else ("mesure BR ",b, unaryGateAt (unQubitId qbit_id) (measure BR) countqbits, OneMesure)

traverseArc  (LoopEdge a string) g1 countqbits =  
                ("notOpp ",1 , identityOnNQB countqbits, Shure)


valueOfBitInState ::LabGraph t t1 -> BitId->Vertex ->Int
valueOfBitInState (LabGraph gr lab) iD s = findIn iD $ lab s

findIn :: BitId -> [(BitId,Int)] -> Int
findIn iD [] = -1  -- error "Value found for qbit index "++ (show iD)++"\n"
findIn iD ((a,b):tx) = if a==iD then b else findIn iD tx  


printRow n lx = writer (n, if(length lx == 0) then "[] s=" ++ (show n) ++ " -> 1.0:(s'=" ++ (show n) ++ ");\n" 
                                              else "[] s=" ++ (show n) ++ " -> "  ++  (vp lx))
        where
            --vp [] = ""
            vp ((p,nn):[]) = (show p) ++ ": (s'=" ++ (show nn) ++ ");\n"
            vp ((p,nn):l:lx) = (show p) ++ ": (s'=" ++ (show nn) ++ ") + " ++ (vp $ l: lx)

enumNodes g1@(LabGraph gr lab)= do
                    writer(numOfQB  {-+1 it counts one less do not know...-},output)
    where
        vert= vertices g1
        numOfQB= length $ lab $ vert!!1
        cou= length vert
        comment= "\n//" ++ (concat $ map (\x -> ", state:"++(show x) ++" BITS : " ++ (concat $ sort $ map (\y -> ", "++ show (fst y) ++ " has value " ++ (show (snd y))) $ lab x) ++ "\n //"  ) vert) ++ "\n"
        output = "s:[1.."++(show cou)++"] init 1;" ++comment



graphToPRISM g1@(LabGraph gr lab) = "\n\n// number of Qubits: "++(show cq)++ "\n\n"++ "dtmc\n module quipksmodel\n "++result ++"\n"++
                        (dfsVisitLabGraph (initialState cq) g1 countqbits) ++"\n endmodule\n"
    where 
        (countqbits,result)= runWriter $ enumNodes g1
        cq = countqbits 

-- The main dfs visit of the Model
dfsVisitLabGraph state g1@(LabGraph gr lab) countqbits =  b 
    where

        notin x = any (x==)
        (a,b) = runWriter $ dfsvg 1 state g1 [1]

        dfsvg node state g@(LabGraph gr lab) visited = do
                                visitedAdjProbs <- mapM (visit visited) edges
                                printRow node visitedAdjProbs 
                                writer ((0.0,node), "")
            where
                visit visited edge =
                        if notin ne visited 
                        then writer(( 1.0, ne), matrixTxt)
                        else do (dfsvg ne newState g (ne:visited)); writer(( pfa, ne), matrixTxt)
                            where
                                matrixTxt = if debugMode then "Apply..."++ name ++ "\n OldState =\n"++ (show state) ++"\n Operator:\n"++ (show m)++"getting....\n"++(show newUnnormalized)++"\n"++"Normalizing....\n"++(show newState)++"\n" else ""
                                (name,ne,m,kind) = traverseArc edge g1 countqbits
                                newState = case kind of Shure -> newUnnormalized
                                                        ZeroMesure -> normalizeStateVector $ newUnnormalized
                                                        OneMesure -> normalizeStateVector $ newUnnormalized
                                newUnnormalized = m*state
                                pfa= case kind of Shure -> 1.0
                                                  ZeroMesure -> getProbabilityOfZERO 1 state
                                                  OneMesure ->  getProbabilityOfONE 1 state
                                                  --otherwise -> 0.0


                edges= gr ! node

instance Show (LabGraph a b) where
    show  =  showGraphV