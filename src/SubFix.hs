{-|
Module      : SubFix
Description : core functions for subfix
Copyright   : (C) Jonathan Lamothe
License     : GPL-3
Maintainer  : jonathan@jlamothe.net

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

{-# LANGUAGE LambdaCase, RecordWildCards #-}

module SubFix (
  -- * Data Types
  Caption (..),
  -- * Functions
  convert,
  decode,
  encode,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Char (chr)

import SubFix.Internal (decodeTime, encodeTime)

-- | Defines a caption group
data Caption = Caption
  { capID :: Int
  -- ^ The caption identifier
  , capStart :: Integer
  -- ^ The caption start time (in milliseconds)
  , capEnd :: Integer
  -- ^ The caption end time (in milliseconds)
  , capText :: String
  -- ^ The caption text
  } deriving (Eq, Show)

-- | Applies the transformations to a caption group
convert :: Caption -> Caption
convert c = c { capText = editText $ capText c }

-- | Decodes the text of a subtitle file
decode
  :: String
  -- ^ The encoded text
  -> Either String [Caption]
  -- ^ The resulting caption list, or a message describing the error
  -- that occured.
decode = evalStateT decodeLoop . lines

-- | Encodes a list of caption groups
encode :: [Caption] -> String
encode []     = ""
encode [c]    = encodeCaption c
encode (c:cs) = encodeCaption c ++ "\n" ++ encode cs

editText :: String -> String
editText = checkCaret . checkNotes

checkCaret :: String -> String
checkCaret ('^' : str) = "{\\an8}" ++ str
checkCaret str = str

checkNotes :: String -> String
checkNotes "" = ""
checkNotes ('#' : '#' : str) = chr 0x2669 : checkNotes str
checkNotes (ch : str) = ch : checkNotes str

decodeLoop :: StateT [String] (Either String) [Caption]
decodeLoop = get >>= \case
  [] -> return []
  _  -> do
    caption  <- decodeNextCaption
    captions <- decodeLoop
    return $ caption : captions

decodeNextCaption :: StateT [String] (Either String) Caption
decodeNextCaption = do
  capID              <- decodeID
  (capStart, capEnd) <- decodeTimes
  capText            <- decodeText
  return $ Caption {..}

decodeID :: StateT [String] (Either String) Int
decodeID = do
  line <- nextLine
  case reads line of
    [(val, "")] -> return val
    _           -> lift $ Left "invalid caption ID"

decodeTimes :: StateT [String] (Either String) (Integer, Integer)
decodeTimes = do
  line <- nextLine
  case words line of
    [startStr, "-->", endStr] -> do
      start <- case decodeTime startStr of
        Just (val, "") -> return val
        _              -> lift $ Left "invalid start time"
      end <- case decodeTime endStr of
        Just (val, "") -> return val
        _              -> lift $ Left "invalid end time"
      return (start, end)
    _ -> lift $ Left "invalid time signature"

decodeText :: StateT [String] (Either String) String
decodeText = get >>= \case
  [] -> return ""
  _  -> do
    line <- nextLine
    if line == ""
      then return ""
      else do
        next <- decodeText
        return $ line ++ "\n" ++ next

nextLine :: StateT [String] (Either String) String
nextLine = get >>= \case
  (line : remaining) -> do
    put remaining
    return line
  [] -> lift $ Left "missing line"

encodeCaption :: Caption -> String
encodeCaption c = unlines $
  [ show $ capID c
  , encodeTime (capStart c) ++ " --> " ++ encodeTime (capEnd c)
  ] ++ lines (capText c)

--jl
