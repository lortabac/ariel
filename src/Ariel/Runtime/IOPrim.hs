module Ariel.Runtime.IOPrim where

import Ariel.Common.IOPrim
import Ariel.Runtime.Types
import qualified Data.Text.IO as Text

runIOPrim :: IOPrim Expr -> IO Expr
runIOPrim (WriteLn (String s)) = Text.putStrLn s >> pure unit
runIOPrim ReadLine = String <$> Text.getLine
runIOPrim _ = error "Invalid IOPrim"
