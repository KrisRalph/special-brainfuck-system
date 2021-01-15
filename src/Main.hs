{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module Main where
-- implements a really stupid brainfuck interepreter

import           Data.Word
import           Data.Maybe
import           Data.Default ( Default(..) )
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.ByteString.Internal (c2w, w2c)
import           Control.Monad.State.Lazy
import           System.Environment (getArgs)
import           Control.Lens

data BFOp
  = BFRight -- >
  | BFLeft  -- <
  | BFIncr  -- +
  | BFDecr  -- -
  | BFOutp  -- .
  | BFRepl  -- ,
  | BFJmpL  -- [
  | BFJmpR  -- ]
  | BFNoOp  -- there is no noop
  deriving (Show, Eq)

data BFState = BFState
  { _pcounter :: Int       -- Program Counter
  , _mpointer :: Int       -- Memory pointer
  , _cells    :: Seq Word8 -- Memory cells
  , _ops      :: Seq BFOp  -- List of ops to run
}
makeLenses 'BFState

instance Show BFState where
  show (BFState pc mp pcells bfOps) =
    "BFState {_pcounter = " <> show pc <>
    ", _mpointer ="         <> show mp <>
    ",\t_cells = "          <> show (Seq.lookup mp pcells) <>
    ",\t_ops = "            <> show (Seq.lookup pc bfOps) <> "\t}"

mkBFState :: BFState
mkBFState = BFState
  { _pcounter=0
  , _mpointer=0
  , _cells=Seq.replicate 30000 0
  , _ops=Seq.Empty
  }

instance Default BFState where
  def = mkBFState

-- slightly modified so that we can just skip comments.
parseChar :: Char -> BFOp
parseChar = \case
  '>' -> BFRight
  '<' -> BFLeft
  '+' -> BFIncr
  '-' -> BFDecr
  '.' -> BFOutp
  ',' -> BFRepl
  '[' -> BFJmpL
  ']' -> BFJmpR
  _   -> BFNoOp

-- wrap explicitly to cure warnings
decr :: Word8 -> Word8
decr 0 = 255
decr x = x - 1

incr :: Word8 -> Word8
incr 255 = 0
incr x = x + 1

parseString :: String -> Seq BFOp
parseString = Seq.fromList . fmap parseChar -- just blow up if it's not a well formed program lol

atMemoryPointer :: BFState -> Maybe Word8
atMemoryPointer s = Seq.lookup (s ^. mpointer) (s ^. cells)

incrPc :: BFState -> BFState
incrPc s = s & pcounter +~ 1

eval :: BFOp -> BFState -> StateT BFState IO ()
eval BFRight s = put $ incrPc s & mpointer +~ 1
eval BFLeft  s = put $ incrPc s & mpointer -~ 1
eval BFIncr  s = put $ incrPc s & cells %~ Seq.adjust incr (s ^. mpointer)
eval BFDecr  s = put $ incrPc s & cells %~ Seq.adjust decr (s ^. mpointer)
eval BFOutp  s = do
  liftIO . putChar . w2c . fromJust . atMemoryPointer $ s
  put $ incrPc s
eval BFRepl  s = do
  c <- liftIO getChar <&> c2w
  put $ s & pcounter +~ 1 & cells %~ Seq.update (s ^. mpointer) c
eval BFJmpL  s = do
  case atMemoryPointer s of
    Just 0 -> do
      let (_, nextOps) = Seq.splitAt (s ^. pcounter) (s ^. ops)
      put $ s & pcounter .~ (ljump nextOps + 1)
    _ -> put $ incrPc s
eval BFJmpR  s = do
  case atMemoryPointer s of
    Just x | x /= 0 -> do
      let (prevOps, _) = Seq.splitAt (s ^. pcounter+1) (s ^. ops)
      let pc' = (s ^. pcounter) - rjump prevOps + 1
      put $ s & pcounter .~ pc'
    _ -> put $ incrPc s
eval BFNoOp s = put $ incrPc s

ljump :: Seq BFOp -> Int
ljump opSeq = fromMaybe 0 (Seq.findIndexL (==BFRight) opSeq)

rjump :: Seq BFOp -> Int
rjump opSeq = fromMaybe 0 $ getOffset 0 0 (Seq.reverse opSeq)

-- dumb scope matcher helper
-- returns the distance offset to the next scoped [, or Nothing 
getOffset :: Int -> Int -> Seq BFOp -> Maybe Int
getOffset offset rpars (BFJmpL :<| rest) = case rpars of
  1 -> Just offset
  _ -> getOffset (offset + 1) (rpars - 1) rest
getOffset offset rpars (BFJmpR :<| rest) = getOffset (offset + 1) (rpars + 1) rest
getOffset offset rpars (_ :<| rest) = case rpars of
  0 -> getOffset offset rpars rest -- we haven't actually seen any '[' yet
  _ -> getOffset (offset + 1) rpars rest
getOffset offset _ Seq.Empty = Just offset

runBFOp :: StateT BFState IO ()
runBFOp = do
  s <- get
  let nextInstr = Seq.lookup (s ^. pcounter) (s ^. ops)
  case nextInstr of
    Just bfOp -> get >>= eval bfOp >> runBFOp
    _ -> liftIO $ putStrLn "\nDone."

runFile :: FilePath -> IO ()
runFile fp = do
  file <- readFile fp
  runString file

runString :: String -> IO ()
runString s = let defState = def {_ops=parseString s} in 
  evalStateT runBFOp defState
  
main :: IO ()
main = do
  args <- getArgs
  forM_ args runFile