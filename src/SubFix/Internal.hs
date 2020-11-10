{-|
Module      : SubFix.Internal
Description : core functions for subfix
Copyright   : (C) Jonathan Lamothe
License     : GPL-3
Maintainer  : jonathan@jlamothe.net

= IMPORTANT:

THIS MODULE IS USED INTERNALLY AND MAY BE SUBJECT TO CHANGE WITHOUT
NOTICE!

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-}

{-# LANGUAGE LambdaCase #-}

module SubFix.Internal (decodeTime, encodeTime, timestamp) where

import Control.Monad (when)
import Control.Monad.Trans.State (StateT (..), get, put)
import Data.Char (isDigit, ord)

-- | Decodes a timestamp to a number of milliseconds and the unused
-- input
decodeTime
  :: String
  -- ^ The encoded timestamp
  -> Maybe (Integer, String)
  -- ^ The number of milliseconds and unused input (if available)
decodeTime = runStateT $ do
  hours <- getNum 2
  dropChar ':'
  mins <- getNum 2
  dropChar ':'
  secs <- getNum 2
  dropChar ','
  ms <- getNum 3
  return $ timestamp hours mins secs ms

-- | Encodes a timestamp from a number of milliseconds
encodeTime :: Integer -> String
encodeTime ms = let
  (s, ms') = ms `divMod` 1000
  (m, s')  = s `divMod` 60
  (h, m')  = m `divMod` 60
  in
    mkNum 2 h ++ ":" ++
    mkNum 2 m' ++ ":" ++
    mkNum 2 s' ++ "," ++
    mkNum 3 ms'

-- | Converts hours, minutes, seconds and milliseconds into the total
-- number of milliseconds
timestamp
  :: Integer
  -- ^ Hours
  -> Integer
  -- ^ Minutes
  -> Integer
  -- ^ Seconds
  -> Integer
  -- ^ Milliseconds
  -> Integer
timestamp h m s ms = let
  m' = h * 60 + m
  s' = m' * 60 + s
  in s' * 1000 + ms

getNum :: MonadFail m => Int -> StateT String m Integer
getNum = f 0 where
  f val digits
    | digits <= 0 = return val
    | otherwise   = do
      digit <- nextDigit
      f (val * 10 + digit) (pred digits)

dropChar :: MonadFail m => Char -> StateT String m ()
dropChar expected = do
  ch <- nextChar
  when (ch /= expected) $
    fail "incorrect character"

nextDigit :: MonadFail m => StateT String m Integer
nextDigit = do
  ch <- nextChar
  if isDigit ch
    then return $ toInteger $ ord ch - ord '0'
    else fail "missing digit"

nextChar :: MonadFail m => StateT String m Char
nextChar = get >>= \case
  ""       -> fail "no character available"
  (ch:str) -> do
    put str
    return ch

mkNum :: Int -> Integer -> String
mkNum digits val =
  reverse $ take digits $ reverse (show val) ++ repeat '0'

--jl
