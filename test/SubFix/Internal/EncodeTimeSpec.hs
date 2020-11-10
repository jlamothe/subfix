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

module SubFix.Internal.EncodeTimeSpec (spec) where

import Test.Hspec (Spec, context, describe, it, shouldBe)

import SubFix.Internal (encodeTime, timestamp)

spec :: Spec
spec = describe "encodeTime" $ mapM_
  ( \(label, input, expected) ->
    context label $
      it ("should be: " ++ expected) $
        encodeTime input `shouldBe` expected
  )

  --  label,           input,                  expected
  [ ( "single digit",  timestamp 1 2 3 4,      "01:02:03,004" )
  , ( "double digits", timestamp 10 11 12 13,  "10:11:12,013" )
  , ( "no padding",    timestamp 10 11 12 123, "10:11:12,123" )
  ]

--jl
