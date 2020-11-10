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

module SubFix.DecodeSpec (spec) where

import Test.Hspec (Spec, context, describe, it, shouldBe)

import SubFix (Caption (..), decode)
import SubFix.Internal (timestamp)

spec :: Spec
spec = describe "decode" $ do
  context "valid inputs" $ mapM_
    ( \(label, input, expected) ->
      context label $ do
        let Right results = decode input

        context "number of results" $ let
          rlen = length results
          elen = length expected
          in it ("should be " ++ show elen) $
            rlen `shouldBe` elen

        mapM_
          ( \(num, r, e) ->
            context ("result #" ++ show num) $
              it ("should be " ++ show e) $
                r `shouldBe` e
          ) $ zip3 [(1::Int)..] results expected
    )

    --  label,            input,         expected
    [ ( "empty",          "",            []             )
    , ( "one caption",    oneCaption,    oneCaption'    )
    , ( "two captions",   twoCaptions,   twoCaptions'   )
    , ( "three captions", threeCaptions, threeCaptions' )
    , ( "multi-line",     multiLine,     multiLine'     )
    ]

  context "invalid inputs" $ mapM_
    ( \(label, input, expected) ->
      context label $ let
        Left result = decode input
        in it ("should be: " ++ expected) $
          result `shouldBe` expected
    )

    --  label,            input,    expected
    [ ( "bad ID",         badID,    badID'    )
    , ( "bad start time", badStart, badStart' )
    , ( "bad end time",   badEnd,   badEnd'   )
    , ( "bad separator",  badSep,   badSep'   )
    ]

  where
    oneCaption = unlines
      [ "1"
      , "01:02:03,004 --> 05:06:07,008"
      , "foo"
      ]

    oneCaption' =
      [ Caption
        { capID    = 1
        , capStart = timestamp 1 2 3 4
        , capEnd   = timestamp 5 6 7 8
        , capText  = "foo\n"
        }
      ]

    twoCaptions = oneCaption ++ unlines
      [ ""
      , "2"
      , "09:10:11,012 --> 13:14:15,016"
      , "bar"
      ]

    twoCaptions' = oneCaption' ++
      [ Caption
        { capID    = 2
        , capStart = timestamp 9 10 11 12
        , capEnd   = timestamp 13 14 15 16
        , capText  = "bar\n"
        }
      ]

    threeCaptions = twoCaptions ++ unlines
      [ ""
      , "3"
      , "17:18:19,020 --> 21:22:23,024"
      , "baz"
      ]

    threeCaptions' = twoCaptions' ++
      [ Caption
        { capID    = 3
        , capStart = timestamp 17 18 19 20
        , capEnd   = timestamp 21 22 23 24
        , capText  = "baz\n"
        }
      ]

    multiLine = unlines
      [ "1"
      , "01:02:03,004 --> 05:06:07,008"
      , "foo"
      , "bar"
      ]

    multiLine' =
      [ Caption
        { capID    = 1
        , capStart = timestamp 1 2 3 4
        , capEnd   = timestamp 5 6 7 8
        , capText  = "foo\nbar\n"
        }
      ]

    badID  = "asdf"
    badID' = "invalid caption ID"

    badStart = unlines
      [ "1"
      , "asdf --> 01:02:03,004"
      ]

    badStart' = "invalid start time"

    badEnd = unlines
      [ "1"
      , "01:02:03,004 --> asdf"
      ]

    badEnd' = "invalid end time"

    badSep = unlines
      [ "1"
      , "01:02:03,004 asdf 05:06:07,008"
      ]

    badSep' = "invalid time signature"

--jl
