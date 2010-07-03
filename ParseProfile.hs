--
-- ParseProfile.hs
--
-- Copyright (c) 2008 Antiope Associates LLC, all rights reserved.
--
--
-- This module parses the profile file generated when a program is run
-- with the +RTS -px -RTS flag.  The result is a Profile record that
-- contains all of the information from the file.
--
-- Although it does not describe the exact format output when using the
-- +RTS -px -RTS flag, the best reference on the cost center stack accounting
-- scheme is
--
--        Morgan, R.G. and S.A. Jarvis, "Profiling large-scale lazy
--            functional programs", J. Functional Prog 8 (3), 1998,
--            pp. 201 - 237.
-- 
-- The format of the profile file generated when using the "-px" flag:
--
-- The first line is a quoted string giving the time of day when the
-- profile was generated:
--
--        <timestamp>
--
-- The second line is a quoted string giving the sampling interval, e.g.,
-- "20 ms":
--
--        <tick interval>
--
-- The profiling data begins with lines listing the names of all the cost
-- centers, and the module to which each belongs. Each line begins with
-- a literal '1':
--
--        1 <cost center number> <cost center name> <module name>
--
-- The <cost center number> is a integer, the <cost center name> and
-- <module name> are quoted strings.
--
-- The call tree itself is given by lines beginning with a literal '2'.
-- Each line looks like:
--
--        2 <cost center stack code> <root code> <cost center number> <parent node>
--
-- The <cost center stack code> is identifies the stack to which this
-- <cost center number> belongs.  The <root code> is a literal '1' if this
-- cost center is the root of the call tree, a literal '2' otherwise.
-- The <parent node> is the cost center stack code of the parent of this
-- cost center.  If the <root code> is '1' the <parent node> field is
-- absent.  The <cost center stack code>, <cost center number> and
-- <parent node> are all integers.
--
-- One important thing to note is that multiple lines can have the same
-- <cost center stack code>.  Only the first entry contains cost accounting
-- data.  The other lines with the same <cost center stack code> are "back edges",
-- used internally by the run time system to efficiently navigate the
-- cost center stack graph, but not containing any additional accounting
-- information.
--
-- The final line of the file contains all of the actual profiling data.
-- The format is:
--
--        5 <total ticks> {1 <cost center stack code> <call count> <tick count> <alloc count>}+ 0
--
-- where the expression in curly braces is repeated for all of the
-- cost center stack codes.  Each cost reports for <cost center stack code>
-- begins with a literal '1'.  The <cost center stack code> and the
-- <call count>, <tick count> and <alloc count> are all integers. The end
-- of the profiling data is indicated by a literal '0'.
--
-- So what's a cost center and what is a cost center stack?  A cost center
-- is simply a section of code to which costs are attributed.  It is
-- often a function, but source code annotations can be used to divide
-- a function into several cost centers.
--
-- For the purpose of understanding the profiling file format, a cost
-- center stack is just a cost center and its calling cost center.
-- Costs --- call counts, ticks spent executing and allocations --- are
-- sttributed to call center stacks.  This means, for example, that if
-- both function_1 and function_2 call map, the costs of map will be
-- in two call center stacks, one with function_1 as the parent of
-- map, and another with function_2 as its parent.  In essence, a
-- call center stack can be thought of as an _edge_ of the call graph,
-- while the cost centers themselves are the nodes.
--
-- Note that the <cost center stack code> may not be unique.  This can
-- happen, for instance,  if there are mutually recursive cost centers.
-- In that case costs, while attributable to a particular node (cost center)
-- can't be attributed to a specific edge (cost center stack).
--
-- Parsing the profile returns a data structure called a ProfileGraph.
-- This is an IntMap keyed by <cost center number>.  The entries in the
-- map are records giving the name of the cost center and the module to
-- which it belongs, and an incidence list giving the nodes that call
-- this cost center and the costs associated with each.
--
-- Note that the conversion from call tree (in which each cost center
-- can appear multiple times) to call graph (where each cost center
-- appears only once) is implicitly done as part of the generation of
-- the ProfileGraph record.
--

module ParseProfile (
        CallInfo(..),
        Node(..),
        Profile(..),
        ProfileGraph,
        markParents,
        parseProfile
) where


import Control.Exception
import Data.IntMap as IntMap
import Data.List as List
import Data.Maybe as Maybe
import Text.ParserCombinators.Parsec as Parsec


-- The CostCenter, CostCenterStack and CostCenterReport records
-- hold the raw results of parsing the profile.
--
data CostCenter =
        CostCenter { ccName   :: String,
                     ccModule :: String }
        deriving (Show)

data CostCenterStack =
        CostCenterStack { childNode   :: Int,
                          parentStack :: Maybe Int }
        deriving (Show)

data CostCenterReport =
        CostCenterReport { reportCount :: Integer,
                           reportTicks :: Integer,
                           reportAlloc :: Integer }
        deriving (Show)


defaultCostCenterReport :: CostCenterReport
defaultCostCenterReport =
       CostCenterReport { reportCount = 0,
                          reportTicks = 0,
                          reportAlloc = 0 }


-- The Profile, ProfileGraph, Node and CallingNode types are
-- used to return the annotated call graph.
--
type EdgeCode = Int
type NodeCode = Int
type ProfileGraph = IntMap Node

data Node =
        Node { nodeNumber  :: Int,
               nodeName    :: String,
               nodeModule  :: String,
               isLeaf      :: Bool,
               parentNodes :: [ CallInfo ] }
        deriving (Show)

data CallInfo =
        CallInfo { parentNodeNumber :: Int,
                   stackNumber      :: Int,
                   counts           :: Integer,
                   ticks            :: Integer,
                   allocs           :: Integer }
        deriving (Show)


data Profile =
        Profile { timestamp    :: String,
                  tickInterval :: String,
                  profileTicks :: Integer,
                  profileGraph :: ProfileGraph }
        deriving (Show)


-- Instances of Eq and Ord for CallInfo are handy for grouping
-- and sorting the parent nodes.
--                  
instance Eq CallInfo where
        (==) c1 c2 | pn1 == pn2 = True
                   | otherwise  = False
                   where
                     pn1 = parentNodeNumber c1
                     pn2 = parentNodeNumber c2


instance Ord CallInfo where
        compare c1 c2 | pn1 == pn2 = EQ
                      | pn1 <  pn2 = LT
                      | otherwise  = GT
                      where
                        pn1 = parentNodeNumber c1
                        pn2 = parentNodeNumber c2


-- The parser
--
costCenterCode :: Parser Char
costCenterCode      = char '1'

costCenterStackCode :: Parser Char
costCenterStackCode = char '2'

timeUpdateCode :: Parser Char
timeUpdateCode      = char '5'

eol :: Parser ()
eol = do
  Parsec.try (do char '\r'; newline) <|> newline
  return ()
  <?> "eol"

natural :: Parser Int
natural = do
  digits <- many1 digit
  return ((read digits) :: Int)
  <?> "natural"

naturalLong :: Parser Integer
naturalLong = do
  digits <- many1 digit
  return ((read digits) :: Integer)
  <?> "naturalLong"

quotedString :: Parser String
quotedString = do
  char '"'
  manyTill anyChar (Parsec.try (char '"'))
  <?> "quotedString"

headerStr :: Parser String
headerStr =  do 
  header <- quotedString; eol
  return header
  <?> "headerString"

costCenter :: Parser (NodeCode, CostCenter)
costCenter = do
  costCenterCode; space
  ccId  <- natural; space
  name  <- quotedString; space
  modul <- quotedString; eol
  return (ccId, CostCenter { ccName   = name,
                             ccModule = modul })
  <?> "costCenter"

costCenterStack :: Parser (EdgeCode, [ CostCenterStack ])
costCenterStack = do
  costCenterStackCode; space
  ccsId     <- natural; space
  stackCode <- oneOf "12"; space
  node      <- natural
  parent    <- do if stackCode == '1'
                      then return Nothing
                      else do
                        space
                        p <- natural
                        return (Just p)
  eol
  return (ccsId, [ CostCenterStack { childNode   = node,
                                     parentStack = parent } ] )
  <?> "costCenterStack"


ccsReport :: Parser (EdgeCode, CostCenterReport)
ccsReport = do
  char '1'; space
  ccsId <- natural; space
  cs    <- naturalLong; space
  ts    <- naturalLong; space
  as    <- naturalLong; space
  return (ccsId, CostCenterReport { reportCount = cs,
                                    reportTicks = ts,
                                    reportAlloc = as })
  <?> "ccsReport"


totalTicks :: Parser Integer
totalTicks = do
  timeUpdateCode; space
  ts <- naturalLong; space
  return ts
  <?> "totalTicks"


profile :: Parser Profile
profile = do
  stamp            <- headerStr
  step             <- headerStr
  costCenters      <- many costCenter
  costCenterStacks <- many costCenterStack
  totalTime        <- totalTicks
  times            <- manyTill ccsReport (Parsec.try (char '0'))
  let
          p = mkProfileGraph costCenters costCenterStacks times

  return Profile { timestamp    = stamp,
                   tickInterval = step,
                   profileTicks = totalTime,
                   profileGraph = p }
  <?> "profile"


-- Helper functions for converting the CostCenter, CostCenterStack
-- and CostCenterReport lists into a ProfileGraph.
--
allSame :: Eq a => [ a ] -> Bool
allSame [] = True
allSame (_ : []) = True
allSame (x : xs) = if x /= head xs then False else allSame xs

 
mkEdge :: IntMap [ CostCenterStack ]
       -> IntMap CostCenterReport
       -> EdgeCode
       -> (NodeCode, [ CallInfo ])
mkEdge edgeMap costMap e = let
        sts      = edgeMap ! e
        children = List.map childNode sts
        child = assert (allSame children) (head children)

        getCallInfo  :: CostCenterStack -> Maybe CallInfo
        getCallInfo st = let
                parent = if isJust (parentStack st)
                         then let offspring = List.map childNode (edgeMap ! (fromJust (parentStack st)))
                                in assert (allSame children) (Just (head offspring))
                         else Nothing
                in
                  if isJust parent
                  then let
                      c = findWithDefault defaultCostCenterReport e costMap
                      in
                        Just CallInfo { parentNodeNumber = fromJust parent,
                                        stackNumber      = e,
                                        counts           = reportCount c,
                                        ticks            = reportTicks c,
                                        allocs           = reportAlloc c }
                  else Nothing
        in
          (child, Maybe.mapMaybe getCallInfo sts)


mkEdges :: [ (EdgeCode, [ CostCenterStack ]) ]
        -> [ (EdgeCode, CostCenterReport) ]
        -> [ (NodeCode, [ CallInfo ]) ]
mkEdges ccss costs = let
        edgeMap = fromListWith (\_ x -> x) ccss
        costMap = fromList costs
        mkEdge' = mkEdge edgeMap costMap
        in
          List.map mkEdge' (keys edgeMap)


edgesToNodes :: [ (NodeCode, CostCenter) ]
             -> [ (NodeCode, [ CallInfo ]) ]
             -> [ (NodeCode, Node) ] 
edgesToNodes nodes edges = let
        nodeMap = fromList nodes
        mkNode :: (NodeCode, [ CallInfo ]) -> (NodeCode, Node)
        mkNode (n, ci) = (n, Node { nodeNumber  = n,
                                    nodeName    = ccName   (nodeMap ! n),
                                    nodeModule  = ccModule (nodeMap ! n),
                                    isLeaf      = True,
                                    parentNodes = ci } )
        in
          List.map mkNode edges


concatParents :: Node -> Node -> Node
concatParents n n' = Node { nodeNumber  = nodeNumber n,
                            nodeName    = nodeName n,
                            nodeModule  = nodeModule n,
                            isLeaf      = (isLeaf n) && (isLeaf n'),
                            parentNodes = (parentNodes n) ++ (parentNodes n') }


-- Mark the Leaf nodes
--
markAll :: ProfileGraph -> ProfileGraph
markAll g = IntMap.map (\n -> n { isLeaf = True }) g

markParents :: ProfileGraph -> ProfileGraph
markParents g = let
        nonLeaf = concatMap (\n -> List.map parentNodeNumber (parentNodes (g ! n))) (keys g)
        g'      = markAll g

        markAsParent :: ProfileGraph -> NodeCode -> ProfileGraph
        markAsParent gr n = update (\n' -> Just n' { isLeaf = False }) n gr
        in
          foldl markAsParent g' nonLeaf


-- Convert the raw CostCenter, CostCenterStack and CostCenterReport
-- lists into a ProfileGraph.
--
mkProfileGraph :: [ (NodeCode, CostCenter) ]
               -> [ (EdgeCode, [ CostCenterStack ] ) ]
               -> [ (EdgeCode, CostCenterReport) ]
               -> ProfileGraph
mkProfileGraph ccs ccss costs = let
        edges = mkEdges ccss costs     -- [ (NodeCode, [ CallInfo ]) ]
        nodes = edgesToNodes ccs edges -- [ (NodeCode, Node) ]
        in
          markParents $ fromListWith concatParents nodes

        
parseProfile :: FilePath -> String -> Maybe Profile
parseProfile name input =
        case parse profile name input of
            Left  _    -> Nothing
            Right prof -> Just prof
