#!/usr/bin/env stack
{-
stack
--install-ghc runghc
--package aeson
--package lens-aeson
--package xml-lens
-}
{-# Language OverloadedStrings #-}
{-# Language FlexibleContexts #-}
import Control.Monad.Reader

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as LB8

import qualified Data.Map as Map
import qualified Data.Vector as V

import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific


import qualified Data.Aeson as Json
import Data.Aeson.Lens

import qualified Text.XML as XML
import Text.XML.Lens

records :: Reader XML.Document [(T.Text, Scientific)]
records =
  let
    field name = nodes . folded . _Element . named "field" . attributeIs "name" name
  in do
    magnify (root . named "Root" ./ named "data" ./ named "record") $ do
      record <- ask
      let name = record ^? (field "Country or Area" . attr "key")
      let year = record ^? (field "Year" . text)
      let val  = record ^? (field "Value" . text)
      return $ case (name, year, val) of
        (Just key, Just "2020", Just val) -> [ (key, read $ T.unpack val) ]
        _ -> []

features :: Map.Map T.Text Scientific -> Reader Json.Value [ Json.Value ]
features popMap = do
  magnify (_Object . ix "features" . _Array . folded) $ do
    feature <- ask
    let Just id = feature ^? (_Object . ix "id" . _String)
    return $ case (Map.lookup id popMap) of
      Just pop -> [ feature & _Object . ix "properties" . _Object . at "pop2020" ?~  Json.Number pop ]
      _ -> [ feature ]


main = do
  xml <- XML.readFile XML.def "population.xml"
  jsonBytes <- LB8.readFile "countries.geo.json"
  let Just json = Json.decode jsonBytes :: Maybe Json.Value
  let pop2020Map = Map.fromList $ runReader records xml
  let featureList = runReader (features pop2020Map) json :: [ Json.Value ]
  let newJson = json & _Object . ix "features"  .~ (Json.Array $ V.fromList featureList)
  LB8.putStrLn $ Json.encode newJson
