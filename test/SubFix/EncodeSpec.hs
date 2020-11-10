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

module SubFix.EncodeSpec (spec) where

import Test.Hspec (Spec, context, describe, it, shouldBe)

import SubFix (Caption (..), encode)
import SubFix.Internal (timestamp)

spec :: Spec
spec = describe "encode" $ mapM_
  ( \(label, input, expected) ->
    context label $
      it ("should encode " ++ show (length input) ++ " records") $
        lines (encode input) `shouldBe` expected
  )

  --  label,           input,        expected
  [ ( "no records",    [],           []            )
  , ( "one record",    oneRecord,    oneRecord'    )
  , ( "two records",   twoRecords,   twoRecords'   )
  , ( "three records", threeRecords, threeRecords' )
  , ( "split caption", multiLine,    multiLine'    )
  ]

  where
    oneRecord =
      [ Caption
        { capID    = 1
        , capStart = timestamp 1 2 3 4
        , capEnd   = timestamp 5 6 7 8
        , capText  = "foo"
        }
      ]

    oneRecord' =
      [ "1"
      , "01:02:03,004 --> 05:06:07,008"
      , "foo"
      ]

    twoRecords = oneRecord ++
      [ Caption
        { capID    = 2
        , capStart = timestamp 9 10 11 12
        , capEnd   = timestamp 13 14 15 16
        , capText  = "bar"
        }
      ]

    twoRecords' = oneRecord' ++
      [ ""
      , "2"
      , "09:10:11,012 --> 13:14:15,016"
      , "bar"
      ]

    threeRecords = twoRecords ++
      [ Caption
        { capID    = 3
        , capStart = timestamp 17 18 19 20
        , capEnd   = timestamp 21 22 23 24
        , capText  = "baz"
        }
      ]

    threeRecords' = twoRecords' ++
      [ ""
      , "3"
      , "17:18:19,020 --> 21:22:23,024"
      , "baz"
      ]

    multiLine =
      [ Caption
        { capID    = 1
        , capStart = timestamp 1 2 3 4
        , capEnd   = timestamp 5 6 7 8
        , capText  = "foo\nbar"
        }
      ]

    multiLine' =
      [ "1"
      , "01:02:03,004 --> 05:06:07,008"
      , "foo"
      , "bar"
      ]

--jl
