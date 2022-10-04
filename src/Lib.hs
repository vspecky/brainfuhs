module Lib
  ( runEitherT
  , startBf
  ) where

import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Char
import Data.Int (Int8)
import qualified Data.Map.Strict as Map
import Data.Sequence hiding (replicate)
import Prelude hiding (lookup)
import System.IO (hFlush, isEOF, stdout)

-- Either Monad Transformer
newtype EitherT e m a =
  EitherT
    { runEitherT :: m (Either e a)
    }

-- Tracks position in a file (1D and 2D)
data Tracker =
  Tracker
    { trckIdx :: Int
    , trckLine :: Int
    , trckCol :: Int
    }

-- Brainfuck state
data Brainfuhs =
  Brainfuhs
    { bfProgram :: Seq Char -- Program
    , bfTracker :: Tracker -- Current position tracker
    , bfTape :: Seq Int8 -- Data tape
    , bfTp :: Int -- Tape (Data) Pointer
    , bfBrackets :: Map.Map Int Tracker
    }

-- Basically an IO (Either String Brainfuhs)
-- Either for graceful exceptions and IO for
-- terminal input/output
type BfState = EitherT String IO Brainfuhs

instance Monad m => Functor (EitherT e m) where
  fmap f ith =
    EitherT $ do
      eith <- runEitherT ith
      case eith of
        Left e -> return $ Left e
        Right s -> return $ Right $ f s

instance Monad m => Applicative (EitherT e m) where
  pure = EitherT . return . Right
  fith <*> vith =
    EitherT $ do
      feith <- runEitherT fith
      veith <- runEitherT vith
      case feith of
        Left e -> return $ Left e
        Right f -> return $ fmap f veith

instance Monad m => Monad (EitherT e m) where
  return = pure
  ith >>= f =
    EitherT $ do
      eith <- runEitherT ith
      case eith of
        Left e -> return $ Left e
        Right s -> runEitherT $ f s

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap return

-- Tape length of 30,000 by default
tapeLen :: Int
tapeLen = 30000

-- Easy accessor for current line
bfPLine :: Brainfuhs -> Int
bfPLine = trckLine . bfTracker

-- Easy accessor for current column
bfPCol :: Brainfuhs -> Int
bfPCol = trckCol . bfTracker

-- Easy accessor for current index in file
bfPc :: Brainfuhs -> Int
bfPc = trckIdx . bfTracker

newTracker :: Tracker
newTracker = Tracker {trckIdx = 0, trckLine = 1, trckCol = 1}

-- Advance tracker by 1 pos in both 1D and 2D
advanceTracker :: Char -> Tracker -> Tracker
advanceTracker '\n' (Tracker idx line _) = Tracker (idx + 1) (line + 1) 1
advanceTracker _ (Tracker idx line col) = Tracker (idx + 1) line (col + 1)

-- Error 'return' for EitherT
returnErr :: Monad m => e -> EitherT e m s
returnErr = EitherT . return . Left

-- Data.Sequence.Seq index lookup with result lifted to EitherT
exceptLookup :: Monad m => e -> Int -> Seq a -> EitherT e m a
exceptLookup err idx seq' =
  case lookup idx seq' of
    Nothing -> returnErr err
    Just a -> return a

-- Error position
appendErrPos :: String -> Brainfuhs -> String
appendErrPos str bf =
  str <> " (" <> show (bfPLine bf) <> ":" <> show (bfPCol bf) <> ")"

-- Read value at current data pointer location
readTape :: Brainfuhs -> EitherT String IO Int
readTape bf = do
  let errMsg = appendErrPos "Overflowed tape" bf
  val <- exceptLookup errMsg (bfTp bf) (bfTape bf)
  return $ fromIntegral val

-- Write value to current data pointer location
writeTape :: Brainfuhs -> Int -> BfState
writeTape bf val =
  let bf' = bf {bfTape = update (bfTp bf) (fromIntegral val) (bfTape bf)}
   in return bf'

-- Jump from a '[' or ']' bracket to the corresponding ']' or '['
-- bracket respectively
jumpBracket :: Brainfuhs -> BfState
jumpBracket bf =
  case Map.lookup (bfPc bf) (bfBrackets bf) of
    Nothing -> returnErr $ appendErrPos "Failed to jump brackets" bf
    Just t -> return bf {bfTracker = t}

-- Execute one BF opcode
execOp :: Brainfuhs -> Char -> BfState
execOp bf op =
  updatePos op $
  case op of
    '.' -> do
      toPrint <- readTape bf
      lift $ putChar $ chr toPrint
      lift $ hFlush stdout
      return bf
    ',' -> do
      eof <- lift isEOF
      if eof
        then return bf
        else do
          input <- lift getChar
          writeTape bf $ ord input
    '<' ->
      if (bfTp bf - 1) < 0
        then return bf {bfTp = tapeLen - 1}
        else return bf {bfTp = bfTp bf - 1}
    '>' ->
      if (bfTp bf + 1) == tapeLen
        then return bf {bfTp = 0}
        else return bf {bfTp = bfTp bf + 1}
    '+' -> do
      val <- readTape bf
      writeTape bf $ val + 1
    '-' -> do
      val <- readTape bf
      writeTape bf $ val - 1
    '[' -> do
      val <- readTape bf
      if val /= 0
        then return bf
        else jumpBracket bf
    ']' -> do
      val <- readTape bf
      if val /= 0
        then jumpBracket bf
        else return bf
    _ -> return bf
  where
    updatePos :: Char -> BfState -> BfState
    updatePos ch bfT = do
      bf' <- bfT
      return bf' {bfTracker = advanceTracker ch $ bfTracker bf'}

-- Run program until completion or exception
runBf :: Brainfuhs -> BfState
runBf bf =
  case lookup (bfPc bf) (bfProgram bf) of
    Nothing -> return bf
    Just op -> execOp bf op >>= runBf

-- Start Brainfuck execution. Memoize all loop start:end positions before
-- executing the program
startBf :: String -> BfState
startBf str = do
  bracketMap <- memoizeBrackets str
  let bf =
        Brainfuhs
          { bfProgram = fromList str
          , bfTracker = newTracker
          , bfTape = fromList $ replicate tapeLen 0
          , bfTp = 0
          , bfBrackets = bracketMap
          }
  runBf bf

-- Memoize positions of corresponding brackets for easy loop jumping
memoizeBrackets :: String -> EitherT String IO (Map.Map Int Tracker)
memoizeBrackets str = EitherT $ return result
  where
    result = go str newTracker [] Map.empty
    getErr :: String -> Tracker -> String
    getErr str' trck =
      str' <> " (" <> show (trckLine trck) <> ":" <> show (trckCol trck) <> ")"
    go ::
         String
      -> Tracker
      -> [Tracker]
      -> Map.Map Int Tracker
      -> Either String (Map.Map Int Tracker)
    go [] _ (t:_) _ = Left $ getErr "Mismatched '['" t
    go (']':_) t [] _ = Left $ getErr "Mismatched ']'" t
    go [] _ [] res = Right res
    go ('[':xs) t ts bMap = go xs (advanceTracker '[' t) (t : ts) bMap
    go (']':xs) t' (t:ts) bMap =
      let t'' = advanceTracker ']' t'
          bMap' = Map.insert (trckIdx t) t' bMap
          bMap'' = Map.insert (trckIdx t') t bMap'
       in go xs t'' ts bMap''
    go (x:xs) t ts bMap = go xs (advanceTracker x t) ts bMap
