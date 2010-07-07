{-# LANGUAGE FlexibleContexts #-}

import Language.Haskell.Exts
import Data.Generics.Uniplate.Data
import Control.Monad.State
import Control.Applicative
import Control.Arrow
import qualified Pretty as P
import qualified Language.Haskell.Exts.Pretty as PP
import ParseProfile
import System.Process
import System.Directory
import System.Environment
import Data.Maybe
import GraphUtils
import Data.Char
import qualified Data.IntMap as IMap
import Text.RegexPR
import Debug.Trace
import Numeric
import Data.List

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
--TODO extract constants
toColour :: Float -> String
toColour fl | 0 <= fl && fl < 0.01 = transparentColour --ignore these values
            | 0.01 <= fl && fl < 0.25 = (pad $ showHex (truncate $ fl * 1024) "") ++ "ff00" --green to yellow
            | 0.25 <= fl && fl <= 1 = "ff" ++ (pad $ showHex (255 - (truncate $ (fl - 0.25) * 340)) "") ++ "00" --yellow to red
    where   
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

computeAllocMap :: Profile -> [(Int, Float)]
computeAllocMap prof = allocFraction $ map (\n -> (read $ nodeName n :: Int, alloc n)) $ filter (isNumber . head . nodeName) $ IMap.elems graph where
    graph = profileGraph prof
    alloc :: Node -> Float
    alloc n = fromInteger $ trd3 $ totalCost $ parentNodes n
    allocFraction l = map (second (/ (sumSnd l))) l
    sumSnd :: [(Int, Float)] -> Float
    sumSnd l = sum $ map snd l

snd3 :: (Integer, Integer, Integer) -> Integer
snd3 (_,x,_) = x

trd3 :: (Integer, Integer, Integer) -> Integer
trd3 (_,_,x) = x

ind l n = if length l > n then Just $ l !! n else Nothing

main :: IO ()
main = do
    --TODO use getOpts
    args <- getArgs
    let mode = args !! 0
    when (not (isPrefixOf "-h" mode || mode == "-px")) (error "mode should be -h or -px")
    let file = args !! 1
    let run = args !! 2
    let programArgs = ind args 3
    let inpFile = ind args 4
    
    --copy the original file
    let bak = file ++ ".bak"
    removeFile bak
    renameFile file bak

    putStrLn "started parsing original file"
    m <- parseModuleFromFile bak
    let tm = assignSCC m

    putStrLn "writing modified file"
    writeFile file $ PP.prettyPrint tm

    let buildStr = "ghc -prof -fforce-recomp -O2 --make " ++ run ++ ".hs && ./" ++ run ++ " +RTS " ++ mode ++ " -RTS"
    let buildStrArgs = buildStr ++ " " ++ fromMaybe "" programArgs ++ " "
    let buildStrInp = maybe buildStrArgs ((buildStrArgs ++ " < ") ++ ) inpFile
    putStrLn buildStrInp
    buildCommand <- runCommand $ buildStrInp
    waitForProcess buildCommand

    putStrLn "parsing profiling results"
    profFile <- readFile $ run ++ ".prof"
    --TODO handle Maybe
    let prof = fromJust $ parseProfile (run ++ ".prof") profFile
    let profMap = compute prof where
        compute = if isPrefixOf "-h" mode then computeAllocMap else computeTicksMap

    putStrLn "printing output html file"
    let html = addColour profMap $ pprint tm
    writeFile (file ++ ".html") html
    renameFile file (file ++ ".scc")
    copyFile bak file
