module Main where

import Control.Applicative
import Data.List
import Data.Maybe
import Safe
import qualified Text.Megaparsec.Char as P

import Lib.Parser

main :: IO ()
main = do
  f <- readFile "input/7"
  let output = parse f
      fs = buildFs output
      result = sizeUpTo100k fs
  print result

-- ** Terminal output

data TerminalOutput
  = Command Command
  | Output Output
  deriving (Show, Eq)

data Command
  = Cd String
  | Ls
  deriving (Show, Eq)

data Output
  = Dir String
  | File Integer String
  deriving (Show, Eq)

outputName :: Output -> String
outputName (Dir nm) = nm
outputName (File _ nm) = nm

parse :: String -> [TerminalOutput]
parse = runParser parseTerminalOutputs

parseTerminalOutputs :: Parser [TerminalOutput]
parseTerminalOutputs = many (parseTerminalOutput <* P.newline)

parseTerminalOutput :: Parser TerminalOutput
parseTerminalOutput = Command <$> parseCommand <|> Output <$> parseOutput

parseCommand :: Parser Command
parseCommand = P.string "$ " *> (parseCd <|> parseLs)

parseCd :: Parser Command
parseCd = Cd <$ P.string "cd " <*> some P.printChar

parseLs :: Parser Command
parseLs = Ls <$ P.string "ls"

parseOutput :: Parser Output
parseOutput = parseDir <|> parseFile

parseDir :: Parser Output
parseDir = Dir <$ P.string "dir " <*> some P.printChar

parseFile :: Parser Output
parseFile = File <$> parseNumber <* P.string " " <*> some P.printChar

-- ** Building directory tree

data FsNode
  = FsDir String [FsNode]
  | FsFile Integer String
  deriving (Show, Eq)

nodeName :: FsNode -> String
nodeName (FsDir nm _) = nm
nodeName (FsFile _ nm) = nm

data FsState = FsState
  { currentDirectory :: FsZipper
  } deriving (Show, Eq)

data FsZipper = FsZipper
  { focus :: Maybe FsNode
  , context :: [Context]
  } deriving (Show, Eq)

data Context = Context
  { before :: [FsNode]
  , after :: [FsNode]
  , dirName :: String
  } deriving (Show, Eq)

toZipper :: FsNode -> FsZipper
toZipper nd = FsZipper { focus = Just nd, context = [] }

up :: FsZipper -> FsZipper
up z = case context z of
  [] -> z
  (ctx:ctxs) -> FsZipper
    { focus = Just $ FsDir (dirName ctx) (reverse (before ctx) ++ maybeToList (focus z) ++ after ctx)
    , context = ctxs
    }

fromZipper :: FsZipper -> FsNode
fromZipper z = case context z of
  [] -> fromJustNote "No focus in root" (focus z)
  _ -> fromZipper (up z)

down :: FsZipper -> FsZipper
down z = case focus z of
  Just (FsDir nm nds) -> FsZipper
    { focus = headMay nds
    , context = Context
        { before = []
        , after = tailDef [] nds
        , dirName = nm
        } : context z
    }
  _ -> z

right :: FsZipper -> FsZipper
right z = case context z of
  [] -> z
  (ctx:ctxs) -> FsZipper
      { focus = headMay (after ctx)
      , context = Context
          { before = maybeToList (focus z) ++ before ctx
          , after = tailDef [] (after ctx)
          , dirName = dirName ctx
          } : ctxs
      }

modifyFocus :: (FsNode -> FsNode) -> FsZipper -> FsZipper
modifyFocus f z = z { focus = f <$> focus z }

buildFs :: [TerminalOutput] -> FsNode
buildFs = fromZipper . foldl' (flip processOneOutput) (toZipper $ FsDir "/" [])

processOneOutput :: TerminalOutput -> FsZipper -> FsZipper
processOneOutput (Command (Cd nm)) = enterOrAdd nm
processOneOutput (Command Ls) = id
processOneOutput (Output o) = modifyFocus (addOutput o)

addOutput :: Output -> FsNode -> FsNode
addOutput _ FsFile{} = error "Cannot add children to file"
addOutput o d@(FsDir nm chs) =
  case find (\ch -> outputName o == nodeName ch) chs of
    Nothing -> FsDir nm (chs ++ [toNode o])
    Just ch -> if toNode o == ch then d else error $ "existing child does not match"

toNode :: Output -> FsNode
toNode (File sz nm) = FsFile sz nm
toNode (Dir nm) = FsDir nm []

enterOrAdd :: String -> FsZipper -> FsZipper
enterOrAdd "/" = toZipper . fromZipper
enterOrAdd ".." = up
enterOrAdd nm = enterOrAddChild nm . down

enterOrAddChild :: String -> FsZipper -> FsZipper
enterOrAddChild nm z =
  case focus z of
    Nothing -> z { focus = Just (FsDir nm []) }
    Just nd ->
      if nm == nodeName nd
      then z
      else enterOrAddChild nm (right z)

-- ** Calculating size

fold :: (Integer -> String -> a) -> (String -> [a] -> a) -> FsNode -> a
fold fFile _ (FsFile sz nm) = fFile sz nm
fold fFile fDir (FsDir nm cs) = fDir nm (map (fold fFile fDir) cs)

sizeUpTo100k :: FsNode -> Integer
sizeUpTo100k = snd .
  fold
    (\sz _ -> (sz, 0))
    (\_ szs -> let mySize = sum (map fst szs)
               in (mySize, (if mySize > 100000 then 0 else mySize) + sum (map snd szs))
    )
