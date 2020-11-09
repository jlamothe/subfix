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

module SubFix.ConvertSpec (spec) where

import Data.Char (chr)

import Test.Hspec (Spec, context, describe, it, shouldBe)

import SubFix (Caption (..), convert)

spec :: Spec
spec = describe "convert" $ mapM_
  ( \(input, expected) -> context (show input) $
    it ("should be " ++ show expected) $
      convert input `shouldBe` expected
  )

  --  input,   expected
  [ ( regular,  regular  )
  , ( raised,   raised'  )
  , ( midCaret, midCaret )
  , ( note,     note'    )
  , ( both,     both'    )
  ]

  where
    regular  = base "foo"
    raised   = base "^foo"
    raised'  = base "{\\an8}foo"
    midCaret = base "foo^bar"
    note     = base "foo##bar"
    note'    = base $ "foo" ++ [chr 0x2669] ++ "bar"
    both     = base "^foo##bar"
    both'    = base $ "{\\an8}foo" ++ [chr 0x2669] ++ "bar"

    base str = Caption
      { capID    = 1
      , capStart = 2
      , capEnd   = 3
      , capText  = str
      }

--jl
