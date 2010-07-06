{-# LANGUAGE FlexibleContexts #-}

import Language.Haskell.Exts
import Data.Generics.Uniplate.Data
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

--assign consecutive numbered SCCs to each Exp_
assignSCC :: Module -> Module
assignSCC m = evalState (transformBiM f m) 0 where
    f e = do
        st <- get
        put (st+1)
        return $ Paren $ scc (show st) $ strip e
    --remove all user defined SCCs
    strip :: Exp_ -> Exp_
    strip (SCCPragma _ e) = e
    strip e = e
    --add SCC at each Exp
    scc :: String -> Exp_ -> Exp_
    scc str e = SCCPragma str e

addColour m s = gsubRegexPRBy (pref ++ ".*?" ++ suf) (\str -> pref ++ sccNumToColour str ++ suf) s where
    --replace SCC number with the colour corresponding to its fraction of ticks
    sccNumToColour :: String -> String
    sccNumToColour str = maybe transparentColour toColour $ lookup (readSCCNumber str) m
    --read the Int after #
    readSCCNumber :: String -> Int
    readSCCNumber str = fst $ head $ reads $ drop (length pref) str
    pref = "color: #"
    suf = "\">"
    transparentColour = "00ffffff"

--map a fraction of the total ticks to a colour
toColour :: Float -> String
toColour fl | 0 <= fl && fl < 0.01 = transparentColour --ignore these values
            | 0.01 <= fl && fl < 0.25 = (pad $ showHex (truncate $ fl * 1024) "") ++ "ff00" --green to yellow
            | 0.25 <= fl && fl <= 1 = "ff" ++ (pad $ showHex (255 - (truncate $ (fl - 0.25) * 512)) "") ++ "00" --yellow to red

--pad with 0 a hex number in [0,255]
pad :: String -> String
pad str = if length str == 1 then "0" ++ str else str

parseModuleFromFile :: FilePath -> IO Module
parseModuleFromFile path = fromParseResult <$> parseFile path

computeTicksMap :: Profile -> [(Int, Float)]
computeTicksMap prof = map (\n -> (read $ nodeName n :: Int, ticksFraction n)) $ filter (isNumber . head . nodeName) $ IMap.elems graph where
    totalTicks = profileTicks prof
    graph = profileGraph prof
    ticksFraction n = (fromInteger $ snd3 $ totalCost $ parentNodes n) / (fromInteger totalTicks)

snd3 :: (Integer, Integer, Integer) -> Integer
snd3 (_,x,_) = x

dir = "/home/dan/projects/haskell/automata/"
file = "Automata/DFA/MyDFA.hs"
run =  "run"

main :: IO ()
main = do
    setCurrentDirectory dir
    let bak = file ++ ".bak"
    renameFile file bak

    putStrLn "started parsing original file"
    m <- parseModuleFromFile bak
    let tm = assignSCC m

    putStrLn "writing modified file"
    writeFile file $ PP.prettyPrint tm

    putStrLn "started profiling"
    profCommand <- runCommand $ "ghc -prof -fforce-recomp -O2 --make " ++ run ++ ".hs && ./" ++ run ++ " +RTS -px -RTS 20000 /usr/share/dict/words"
    waitForProcess profCommand

    putStrLn "parsing profiling results"
    profFile <- readFile $ run ++ ".prof"
    --TODO handle Maybe
    let prof = fromJust $ parseProfile (run ++ ".prof") profFile
    let ticksMap = computeTicksMap prof
    let html = addColour ticksMap $ pprint tm
    writeFile (file ++ ".html") html
    copyFile bak file
