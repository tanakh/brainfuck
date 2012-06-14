{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad.Loops
import Control.Monad.State.Strict
import qualified Data.Attoparsec.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.List.Zipper
import System.Environment

newtype BF a = BF { unBF :: StateT (Zipper Int) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Zipper Int))

runBF :: BF a -> IO a
runBF m = evalStateT (unBF m) $ fromList $ replicate 30000 0

bf :: A.Parser (BF ())
bf = (sequence_ <$>) .  many $
      A.char '>' *> return (modify right)
  <|> A.char '<' *> return (modify left)
  <|> A.char '+' *> return (modify $ \z -> replace (cursor z + 1) z)
  <|> A.char '-' *> return (modify $ \z -> replace (cursor z - 1) z)
  <|> A.char '.' *> return (gets cursor >>= liftIO . putChar . toEnum)
  <|> A.char ',' *> return (liftIO getChar >>= modify . replace . fromEnum)
  <|> whileM_ ((/= 0) <$> gets cursor) <$> (A.char '[' *> bf <* A.char ']')
  <|> A.notChar ']' *> bf

main :: IO ()
main = do
  [file] <- getArgs
  progn <- B.readFile file
  case A.parseOnly bf progn of
    Left err -> error err
    Right p -> runBF p
