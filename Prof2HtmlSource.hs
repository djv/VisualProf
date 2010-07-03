{-# LANGUAGE FlexibleContexts #-}

import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Data.Generics.Uniplate.Data
import Control.Monad
import Control.Monad.State
import Control.Applicative
import qualified Pretty as P
import qualified Language.Haskell.Exts.Pretty as PP
import ParseProfile
import System.Process
import Data.Maybe
import GraphUtils
import Data.Char
import qualified Data.IntMap as IMap

type Exp_ = Exp
pprint tm = "<pre>" ++ P.prettyPrint tm ++ "</pre>"

u :: Biplate a (Exp_) => a -> [Exp_]
u m = universeBi m

par :: Exp_ -> Exp_
par e = Paren e

scc :: String -> Exp_ -> Exp_
scc str e = SCCPragma str e

--assign consecutive numbered SCCs to each Exp_
assignSCC m = evalState (transformBiM f m) 0 where
    f e = do
        st <- get
        put (st+1)
        return $ par $ scc (show st) e

parseModuleFromFile path = fromParseResult <$> parseFile path

snd3 (_,x,_) = x

main = do
    m <- parseModuleFromFile "test.hs"
    let tm = assignSCC m
    writeFile "test_prof.hs" $ PP.prettyPrint tm
    profCommand <- runCommand "ghc -prof -fforce-recomp -O2 --make test_prof.hs && ./test_prof +RTS -px"    
    waitForProcess profCommand
    profCont <- readFile "test_prof.prof"
    --TODO handle Maybe
    let prof = fromJust $ parseProfile "test_prof.prof" profCont
    let totalTicks = profileTicks prof
    let graph = profileGraph prof
    let ticksMap = map (\n -> (read $ nodeName n :: Int, (fromInteger $ snd3 $ totalCost $ parentNodes n) / (fromInteger totalTicks))) $ filter (isNumber . head . nodeName) $ IMap.elems graph
    writeFile "test.html" $ pprint tm
