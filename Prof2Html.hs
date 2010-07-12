{-# LANGUAGE FlexibleContexts #-}
module Main (main) where
import Language.Haskell.Exts
import Data.Generics.Uniplate.Data
import Control.Monad.State
import Control.Applicative
import Control.Arrow
import qualified Control.Exception as Exc
import qualified Pretty as P
import qualified Language.Haskell.Exts.Pretty as PP
import ParseProfile
import System.Process
import System.Directory
import System.Environment
import System.IO.Error
import Data.Maybe
import GraphUtils
import Data.Char
import qualified Data.IntMap as IMap
import Text.RegexPR
import Debug.Trace
import Numeric
import Data.List
 
type Exp_ = Exp
pprint tm
  = "<style> a.info{    position:relative;    z-index:24;    color:#000;    text-decoration:none}  a.info:hover{z-index:25;} a.info span{display: none} a.info:hover span{ /*the span will display just on :hover state*/    display:block;    position:absolute;    top:2em; left:2em; width:15em;    border:1px solid #0cf;    background-color:#cff; color:#000;    text-align: center} </style><pre>"
      ++ P.prettyPrint 120 tm ++ "</pre>"
 
assignSCC :: Module -> Module
assignSCC m = evalState (transformBiM f m) 0
  where f e
          = do st <- get
               put (st + 1)
               return $ Paren $ SCCPragma (show st) $ strip e
        strip :: Exp_ -> Exp_
        strip (SCCPragma _ e) = e
        strip e = e
         
addColour m s
  = gsubRegexPRBy (pref ++ ".*?" ++ suf)
      (\ str -> pref ++ sccNumToColour str ++ suf)
      s
  where  
        sccNumToColour :: String -> String
        sccNumToColour str
          = maybe transparentColour toColour $ lookup (readSCCNumber str) m
         
        readSCCNumber :: String -> Int
        readSCCNumber str = fst $ head $ reads $ drop (length pref) str
        pref = "color: #"
        suf = "\">"
transparentColour = "00ffffff"
 
toColour :: Float -> String
toColour fl
  | 0 <= fl && fl < 1.0e-2 = transparentColour
  | 1.0e-2 <= fl && fl < yellow =
    (pad $ showHex (truncate $ fl * 255 * (1 / yellow)) "") ++ "ff00"
  | yellow <= fl && fl <= 1 =
    "ff" ++
      (pad $
         showHex (255 - (truncate $ (fl - yellow) * 255 / (1 - yellow))) "")
        ++ "00"
  where  
        pad :: String -> String
        pad str = if length str == 1 then '0':str else str
        yellow = 0.1
 
parseModuleFromFile :: FilePath -> IO Module
parseModuleFromFile path = fromParseResult <$> parseFile path
 
computeTicksMap :: Profile -> [(Int, Float)]
computeTicksMap prof
  = map (\ n -> (read $ nodeName n :: Int, ticksFraction n)) $
      filter (isNumber . head . nodeName) $ IMap.elems graph
  where totalTicks = profileTicks prof
        graph = profileGraph prof
        ticksFraction n
          = (fromInteger $ snd3 $ totalCost $ parentNodes n) /
              (fromInteger totalTicks)
 
computeAllocMap :: Profile -> [(Int, Float)]
computeAllocMap prof
  = allocFraction $
      map (\ n -> (read $ nodeName n :: Int, alloc n)) $
        filter (isNumber . head . nodeName) $ IMap.elems graph
  where graph = profileGraph prof
         
        alloc :: Node -> Float
        alloc n = fromInteger $ trd3 $ totalCost $ parentNodes n
        allocFraction l = map (second (/ sumSnd l)) l
         
        sumSnd :: [(Int, Float)] -> Float
        sumSnd l = sum $ map snd l
 
snd3 :: (Integer, Integer, Integer) -> Integer
snd3 (_, x, _) = x
 
trd3 :: (Integer, Integer, Integer) -> Integer
trd3 (_, _, x) = x
ind l n = if length l > n then Just $ l !! n else Nothing
 
main :: IO ()
main
  = do args <- getArgs
       let mode = args !! 0
       unless ("-h" `isPrefixOf` mode || "-px" `isPrefixOf` mode)
         (error "mode should be -h or -px")
       let file = args !! 1
       let run = args !! 2
       let programArgs = ind args 3
       let inpFile = ind args 4
       let bak = file ++ ".bak"
       (do removeFile bak `catch`
             (\ e -> if isDoesNotExistError e then return () else ioError e)
           renameFile file bak
           putStrLn "started parsing original file"
           m <- parseModuleFromFile bak
           let tm = assignSCC m
           putStrLn "writing modified file"
           writeFile file $ PP.prettyPrint tm
           let buildStr
                 = "ghc -prof -O2 --make " ++
                     run ++ ".hs && ./" ++ run ++ " +RTS " ++ mode ++ " -RTS"
           let buildStrArgs
                 = buildStr ++ " " ++ fromMaybe "" programArgs ++ " "
           let buildStrInp
                 = maybe buildStrArgs ((buildStrArgs ++ " < ") ++) inpFile
           putStrLn buildStrInp
           buildCommand <- runCommand buildStrInp
           waitForProcess buildCommand
           (when ("-px" `isPrefixOf` mode) $
              do putStrLn "parsing profiling results"
                 profFile <- readFile $ run ++ ".prof"
                 let prof = fromJust $ parseProfile (run ++ ".prof") profFile
                 let profMap = computeTicksMap prof
                 putStrLn "printing output html file"
                 let html = addColour profMap $ pprint tm
                 writeFile (file ++ ".html") html)
           renameFile file (file ++ ".scc"))
         `Exc.finally`
         (do copyFile bak file
             removeFile bak)
