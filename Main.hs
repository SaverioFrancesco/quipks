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
      
      writeFile (f ++ ".pm") $ (graphToPRISM $ labGraphFromTree $ tree)
      writeFile ( g++ ".dot") $ (showGraphViz $ labGraphFromTree $ tree)
      
       where
            g= labGraphFromTree tree
            tree = circToTree inputCirc
            -- inputCirc must be defined into Examples.hs
            -- inputCirc :: (Qubit, ... , Qubit) -> Circ RecAction
            
    