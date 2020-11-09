{-

subfix
Copyright (C) Jonathan Lamothe <jonathan@jlamothe.net>

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

module SubFix.Internal.TimestampSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import SubFix.Internal (timestamp)

spec :: Spec
spec = describe "timestamp" $ let
  expected
    = 1 * 60 * 60 * 1000
    + 2 * 60 * 1000
    + 3 * 1000
    + 4
  in it ("should be " ++ show expected) $
    timestamp 1 2 3 4 `shouldBe` expected

--jl
