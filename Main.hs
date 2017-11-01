{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import System.Environment
import Examples
import Transitions

import Graph
import           Data.Matrix hiding (identity, matrix, zero, (<->), (<|>))
import qualified Data.Matrix

import           Complex
import qualified GatesMatrices
import           QMatrixQuipks
import Dtmc 
import Control.Monad.Writer 

main :: IO ()
main = do
      --[f,g] <- getArgs
       --s   <- readFile f
      --putStrLn $ show  $ -- normalizeStateVector $ (unaryGateAt (1) (measure BR) 2 )* (hadamard `kronecker` (identity 2)) * (e1C `kronecker` e1C)  -- ((hadamard)`kronecker`(identity 2))*  (cGateMulti [1] [2] (pauliX::Matrix (Complex  Float)) 2 ) -- gateMulti [2] (pauliX::Matrix (Complex Float)) 3 --(cGate 1 pauliX) --( swap_multi  [2,1,0]   ) 
      writeFile "model.pm" $ (graphToPRISM $ labGraphFromTree $ tree)
      writeFile "model.dot" $ (showGraphViz $ labGraphFromTree $ tree)
      --(showGraphViz $ labGraphFromTree tree) ++ "\n \n \n"++
      --putStrLn $ show $ concatMap (\x -> "P_"++(show $ snd x)++" = 1 is"++ (show $ fst x) ++ ",  ") [(getProbabilityOfONE i var, i) | i <-[1..2]]
      --putStrLn $ show $ concatMap (\x -> "P_"++(show $ snd x)++" = 0 is "++ (show $ fst x) ++ ",  ") [(getProbabilityOfZERO  i var, i) | i <-[1..4]]

      --putStrLn $  dfsVisitLabGraph e1C $ labGraphFromTree $ tree
      --snd $ runWriter $ printRow 4 ([]::[(Float,Int)])

       where
            g= labGraphFromTree tree

            id2 ::  Matrix  (Complex Float)  
            id2=identity 2 

            m::   Matrix (Complex Float)
            m = kronecker id2 pauliY

            var:: Matrix (Complex Float)
            var = (cnot `kronecker` cnot)  * ( hadamard `kronecker` (identity 2) `kronecker` hadamard `kronecker` (identity 2) ) * (e1C `kronecker` e1C `kronecker` e1C `kronecker` e1C) 

            tree = circToTree circ
            circ = myRnot

             --recCirc'
            --groverSix
            --testMatrix_3  
            --test_if --recCirc'
    