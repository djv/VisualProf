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
import System.Directory
import Data.Maybe
import GraphUtils
import Data.Char
import qualified Data.IntMap as IMap
import Text.RegexPR
import Debug.Trace
import Numeric

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
        return $ par $ scc (show st) $ strip e
    strip (SCCPragma _ e) = e
    strip e = e

addColor m s = gsubRegexPRBy "color: #.*?\">" (\str -> take 8 str ++ g str) s where
    g str =  (maybe "00ffffff" toColor $ lookup (fst $ f str) m) ++ (snd $ f str)
    f str = head $ reads $ drop 8 str :: (Int, String)

toColor :: Float -> String
toColor fl | fl < 0.01 = "00ffffff"
           | otherwise = "ff" ++ (pad $ showHex (255 - (truncate $ fl * 255)) "") ++ "00"

pad str = if length str == 1 then "0" ++ str else str

parseModuleFromFile path = fromParseResult <$> parseFile path

snd3 (_,x,_) = x

dir = "/home/dan/projects/haskell/automata/"
file = "Automata/DFA/MyDFA"
run =  "run"

main = do
    saveDir <- getCurrentDirectory
    setCurrentDirectory dir

    m <- parseModuleFromFile $ file ++ ".hs"
    let tm = assignSCC m
    writeFile (file ++ "Prof.hs") $ PP.prettyPrint tm

    profCommand <- runCommand $ "ghc -prof -fforce-recomp -O2 --make " ++ run ++ ".hs && ./" ++ run ++ " 20000 test +RTS -H100m -A50m -px"
    waitForProcess profCommand
    profFile <- readFile $ run ++ ".prof"
    --TODO handle Maybe
    let prof = fromJust $ parseProfile (run ++ ".prof") profFile
    let totalTicks = profileTicks prof
    let graph = profileGraph prof
    let ticksMap = map (\n -> (read $ nodeName n :: Int, (fromInteger $ snd3 $ totalCost $ parentNodes n) / (fromInteger totalTicks))) $ filter (isNumber . head . nodeName) $ IMap.elems graph
    let html = addColor ticksMap $ pprint tm
    writeFile "test.html" html
