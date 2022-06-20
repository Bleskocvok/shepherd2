#!/usr/bin/env runghc

{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleContexts #-}


module Main where

-- standard imports
import Data.Maybe
import GHC.Generics
import Control.Monad
import System.IO ( hPutStrLn, stderr )
import Data.Functor ( (<&>) )
import Data.Char ( isSpace )
import Data.Text as T ( pack, Text, append )
import Control.Monad.IO.Class ( liftIO )
import Data.ByteString.Char8 as C8 ( pack )
import Data.ByteString.Lazy.Char8 as LC8 ( pack )
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.UTF8 as BSU
-- import qualified Data.ByteString.Lazy.UTF8 as BLS

-- other libraries
import Data.Aeson
import Graphics.Svg


-- # Helper functions

-- inefficient: maybe!
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace


-- # Main functions

main :: IO ()
main = do
    input <- getContents
    let params = eitherDecode' $ LC8.pack input :: Either String Params 
    case params of
        Left err -> putStrLn $ "ERROR: json error\n" ++ err
        Right p -> do putStrLn $ renderSvg p
                      hPutStrLn stderr "graph graphing"
                      hPutStrLn stderr (renderSvg p)


data Params = Params { values :: [Int]
                     , name :: String
                     , desc :: Maybe String
                     }
    deriving (Generic, Show)


instance ToJSON Params where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Params


renderSvg :: Params -> String
renderSvg params = show $ svg $ visualize (values params)
                                <> addText (name params)
                                <> addDesc (maybe "" id (desc params))


pckShow :: Show a => a -> T.Text
pckShow = T.pack . show


graphSize :: (Int, Int)
graphSize = (400, 200)


bgColor, fgColor, txtColor :: T.Text
bgColor = "#DDDDDD"
fgColor = "#38a660"
txtColor = "#121212"


colTopPad, colBotPad, colMinH, colMaxH :: (Num a, Show a) => a
colTopPad = 10
colBotPad = 10
colMinH   = 5
colMaxH   = 100 - colTopPad - colBotPad - colMinH

colWCoeff :: Fractional a => a
colWCoeff = 0.75

colTxtShift, colTxtSize :: (Num a, Show a) => a
colTxtPosCoeff :: Fractional a => a
colTxtShift = 0
colTxtSize  = 8
colTxtPosCoeff = 0.875


getPercent :: (Fractional b, Integral a) => a -> a -> b
getPercent n max = (fromIntegral n) * 100 / fromIntegral max


getShift :: Fractional a => a -> a
getShift prt = fromIntegral (colTopPad + colMaxH)
               - fromIntegral colMaxH / 100 * (prt / 2)
-- getShift prt = 10 + 85 - 0.85 * (prt / 2)


percent :: T.Text -> T.Text
percent = (`T.append` T.pack "%")


addText :: String -> Element
addText txt = text_
            [ X_ <<- "5%"
            , Y_ <<- "13%"
            , Font_size_ <<- "20"
            , Text_anchor_ <<- "left"
            -- , Fill_opacity_ <<- "0.5"
            , Fill_ <<- txtColor
            ] (toElement $ T.pack txt)


addDesc :: String -> Element
addDesc txt = text_
            [ X_ <<- "5%"
            , Y_ <<- "25%"
            , Font_size_ <<- "10"
            , Text_anchor_ <<- "left"
            , Fill_ <<- txtColor
            ] (toElement $ T.pack txt)



columns :: (Integral a, Show a) => [a] -> Element
columns vals = foldr column mempty [0 .. len - 1]
    where
        float = fromIntegral
        largest = maximum vals
        lowest  = minimum vals
        len = length vals
        colW = float (fst graphSize + 1) / float (len + 1)
        column i = (<> text_
                    [ X_            <<- (pckShow $ colW * colTxtPosCoeff + colW * float i)
                    -- , Y_            <<- (percent $ pckShow $ getShift prt - colTxtShift)
                    , Y_            <<- (percent $ pckShow $ 100 - colBotPad + colTxtSize * 0.5)
                    , Font_size_    <<- pckShow colTxtSize
                    , Text_anchor_  <<- "middle"
                    , Fill_opacity_ <<- "0.75"
                    , Fill_         <<- txtColor
                    ] (toElement $ T.pack $ show value))
                    .
                    (<> rect_
                    [ Y_      <<- (percent $ pckShow $ getShift prt)
                    , X_      <<- (pckShow $ colW * 0.5 + colW * float i)
                    , Width_  <<- (pckShow $ colW * colWCoeff)
                    , Height_ <<- (pckShow $ colMinH + prt * colMaxH / 100)
                    , Fill_   <<- fgColor
                    ])
            where prt = getPercent value largest
                  value = (vals !! i)


svg :: Element -> Element
svg con = doctype <> with (svg11_ con)
                    [ Version_ <<- "1.1"
                    , Width_   <<- T.pack (show $ fst graphSize)
                    , Height_  <<- T.pack (show $ snd graphSize)
                    ]


visualize :: (Integral a, Show a) => [a] -> Element
visualize = bg . columns
    where
        bg = (rect_ [
                    Width_    <<- "100%"
                    , Height_ <<- "100%"
                    , Fill_   <<- bgColor
                    ] <>)
