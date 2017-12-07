{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

Author: Luca Foschiani (.dot tranlsator) http://www.graphviz.org/

Author: Francesco Saverio Comisso (PRISM .pm file translator) http://www.prismmodelchecker.org/

-}



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
      [f,g] <- getArgs
      
      --putStrLn $ show $ cN312_v2 
      -- h3* h2*h1* x3* x2*x1* h2* cN23 * h2* x3* x2*x1* h3* h2*h1 *cN312 * h3 * h2 * h1 *x3 *(initialState 3)

      --[(getProbabilityOfZERO i q, getProbabilityOfONE i q)  | i <-[1,2,3] ]
      -- normalizeStateVector $ (m3_0 *) $ normalizeStateVector $ (m2_0*) $ normalizeStateVector $ m1_1 * 
      

      --writeFile (f ++ ".pm") $
      putStrLn $ (graphToPRISM $ labGraphFromTree $ tree)
      -- show $ [getProbabilityOfONE i $(unaryGateAt i (measure BR) 2)* h1 * x1 * (initialState 2)| i <- [1..2]]
      --( g++ ".dot") $
      --putStrLn $  (showGraphViz $ labGraphFromTree $ tree)
      --show $ [getProbabilityOfONE i $(unaryGateAt i (measure BR) 2)*(unaryGateAt i (measure BR) 2)* (unaryGateAt i (measure BR) 2)*  h1 * x1 * (initialState 2)| i <- [1..2]]--
       where
            q= h3*h2 * (initialState 3)
            

            --cN312 :: Matrix (Complex Float)
            h1 :: Matrix (Complex Double)
            h2 :: Matrix (Complex Double)
            h3 :: Matrix (Complex Double)
            x1 :: Matrix (Complex Double)
            x2 :: Matrix (Complex Double)
            x3 :: Matrix (Complex Double)
            m1_0:: Matrix (Complex Double)
            m1_1:: Matrix (Complex Double)
            m2_0:: Matrix (Complex Double)
            m2_1:: Matrix (Complex Double)
            m3_0:: Matrix (Complex Double)
            m3_1:: Matrix (Complex Double)
            cN312_v2:: Matrix (Complex Double)
             
            h1= swapSqrt --unaryGateAt 1 hadamard 3
            h2= unaryGateAt 2 hadamard 3
            h3= unaryGateAt 3 hadamard 3

            x1= unaryGateAt 1 pauliX 2
            x2= unaryGateAt 2 pauliX 3
            x3= unaryGateAt 3 pauliX 3
            --cN312=  cGateMulti [1] [2,3] pauliX 3
            --cN23=  cGateMulti [2] [3] pauliX 3
            cN312_v2 = cGateMulti [1] [6] pauliX 6

            -- gateMulti [3,2] (Dtmc.nameToMatrix "cnot") 3 
            --- unaryGateAt 1 hadamard 1--swap_multi [2,1] --(gateMulti [1,2] (cnot:: Matrix (Complex Float)) 3) :: Matrix (Complex Float)

            m1_0 = unaryGateAt 1 (measure UL) 2
            m1_1 = unaryGateAt 1 (measure BR) 2
            
            m2_0 = unaryGateAt 2 (measure UL) 2
            
            m2_1 = unaryGateAt 2 (measure BR) 2
            
            m3_0 = unaryGateAt 3 (measure UL) 3
            
            m3_1 = unaryGateAt 3 (measure BR) 3
            


            g= labGraphFromTree tree
            tree = circToTree inputCirc
            -- inputCirc must be defined into Examples.hs
            -- inputCirc :: (Qubit, ... , Qubit) -> Circ RecAction
            
    