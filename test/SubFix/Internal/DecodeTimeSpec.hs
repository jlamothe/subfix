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

module SubFix.Internal.DecodeTimeSpec (spec) where

import Test.Hspec (Spec, context, describe, it, shouldBe)

import SubFix.Internal (decodeTime, timestamp)

spec :: Spec
spec = describe "decodeTime" $ mapM_
  ( \(input, expected) ->
    context input $
      it ("should be " ++ show expected) $
        decodeTime input `shouldBe` expected
  )

  --  input,             expected
  [ ( "",                Nothing                            )
  , ( "abc",             Nothing                            )
  , ( "00:00:00,000",    Just (0, "")                       )
  , ( "01:02:03,004",    Just (timestamp 1 2 3 4, "")       )
  , ( "1:2:3,4",         Nothing                            )
  , ( "10:20:30,400",    Just (timestamp 10 20 30 400, "" ) )
  , ( "01:02:03,004abc", Just (timestamp 1 2 3 4, "abc")    )
  ]

--jl
