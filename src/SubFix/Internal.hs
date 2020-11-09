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

module SubFix.Internal (encodeTime, decodeTime, timestamp) where

-- | Encodes a timestamp from a number of milliseconds
encodeTime :: Integer -> String
encodeTime = undefined

-- | Decodes a timestamp to a number of milliseconds and the unused
-- input
decodeTime
  :: String
  -- ^ The encoded timestamp
  -> Maybe (Integer, String)
  -- ^ The number of milliseconds and unused input (if available)
decodeTime = undefined

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

--jl
