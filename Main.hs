{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Csv as C
import Data.Vector as V
import Data.Aeson as A
import Data.List (nub)
import Data.Monoid ((<>))

import GHC.Generics

data DataPoint = DataPoint 
     { name :: Text
     , mean :: Double
     , meanLB :: Double
     , meanUB :: Double
     , stddev :: Double
     , stddevLB :: Double
     , stddevUB :: Double
     } deriving (Show, Generic)


instance C.FromRecord DataPoint

main :: IO ()
main = do
  bs <- LBS.getContents 
  case C.decode C.HasHeader bs of
    Left msg -> error msg
    Right v -> main2 (V.toList v)

main2 :: [DataPoint] -> IO ()
main2 dps = do
  let labels = nub
        [ T.reverse $ T.dropWhile (== '/') . T.dropWhile (/= '/') $ T.reverse $ name dp
        | dp <- dps
        ]
  let plotData = toJSON
        [ let dat :: [DataPoint]
              dat = [ dp | dp <- dps, (label <> "/") `T.isPrefixOf` name dp ]
              xs  :: [Double]
              xs  = [ read $T.unpack $ T.reverse $ T.takeWhile (/= '/') $ T.reverse $ name dp  | dp <- dat ]
              
              ys  :: [Double]
              ys  = [ mean dp  | dp <- dat ]
              
          in object 
                [ "name" .= label
                , "x" .= xs
                , "y" .= ys
                , "mode" .= ("scatter" :: Text)
                ]
{-        
        var trace1 = {
          x: [100, 200, 400, 800],
          y: [1.7092403982086195e-5, 3.460241940566547e-5, 8.595356283028026e-5, 2.257054760093179e-4],
          error_y: {
            type: 'data',
            symmetric: false,
            array: [0,0,0,0],
            arrayminus: [0.2, 0.4, 1, 0.2]
          }, 
          mode: 'scatter',
          name: "left associated monadic binds"
-}
    

        | label <- labels
        ]


  let max_x = Prelude.maximum [ read $ T.unpack $ T.reverse $ T.takeWhile (/= '/') $ T.reverse $ name dp  | dp <- dps ]

  let max_y = Prelude.maximum [ meanUB dp  | dp <- dps ]

  let plotLayout = object
        [ "title"  .= ("Criterion Output" :: Text)
        , "height" .= (500 :: Int)
        , "width"  .= (1000 :: Int)
        , "xaxis"  .= object ["range" .= [0,max_x::Double]]
        , "yaxis"  .= object ["range" .= [0,max_y::Double]]
        ]

  LBS.putStr htmlHeader
  
  LBS.putStr "var data = ";
  LBS.putStr $ A.encode plotData
  LBS.putStr ";\n"

  LBS.putStr "var layout = "
  LBS.putStr $ A.encode plotLayout
  LBS.putStr ";\n"
  
  LBS.putStr "Plotly.newPlot('myDiv', data, layout);\n"

  LBS.putStr htmlFooter
  

htmlHeader = LBS.concat
  ["<head>"
  ,"   <script src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>"
  ,"</head>"
  ,"<body>"
  ,"  <div id=\"myDiv\"></div>"
  ,"  <script>"
  ]

htmlFooter = LBS.concat  
  [ "  </script>"
  , "</body>"
  ]



