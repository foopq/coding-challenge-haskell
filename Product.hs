-----------------------------------------------------------------------------
--
-- Module      :  Product
-- Copyright   :  Copyright (C) 2012 Chris A. Mennie
-- License     :  Released under the GPL version 3 license. See the included LICENSE.
--
-- Maintainer  :  Chris Mennie (chris at chrismennie.ca)
--
-- Sortable-Challenge -- An answer to the Sortable coding challenge
-- Written by Chris Mennie (chris at chrismennie.ca)
--
-----------------------------------------------------------------------------

module Product (
    Product,
    product_name,
    product_manufacturer,
    product_family,
    product_model,
    product_originalJSON,
    product_index,
    readProducts,
    getProductManufacturers
) where


import Text.JSON
import Data.Either
import InputDataCommon
import qualified Data.Set as DS
import Control.DeepSeq


data Product = Product {
                    product_name :: String,
                    product_manufacturer :: [String],
                    product_model :: [String],
                    product_family :: [String],
                    -- product_announced_date :: [String],
                    product_originalJSON :: String,
                    product_index :: Int
                } deriving (Show)


instance NFData Product where
    rnf (Product n ma f mo o i) = rnf n `seq` rnf ma `seq` rnf f `seq` rnf mo `seq` rnf o `seq` rnf i


--instance Show Product where
--    show (Product ma f mo o i) =
--        "[Product " ++ o ++ "]"


-- Given products.txt as a string, parse the products into a list of Products
readProducts :: String -> [Product]
readProducts productFileContents =
        unwrapResultedProducts resultedProducts
    where
        resultedProducts = map readSingleProduct indexedFileLines
        indexedFileLines = zipWith (\l i -> (l, i)) fileLines [1..]
        fileLines = DS.toList . DS.fromList . lines $ productFileContents   -- use set to remove duplicate lines


-- Convert our Product list into one that's escaped from the Result monad
unwrapResultedProducts :: [Result Product] -> [Product]
unwrapResultedProducts resultedListings =
        rights eitherListings
    where
        eitherListings = map Text.JSON.resultToEither resultedListings


-- Parse the JSON into a Product
readSingleProduct :: (String, Int) -> Result Product
readSingleProduct jsonLine = do
    jsonObject <- decodeJsonLine $ fst jsonLine
    return $ getProductFromJSObjectValue jsonObject jsonLine


-- Convert the decoded JSON object into a Product
getProductFromJSObjectValue :: JSObject JSValue -> (String, Int) -> Product
getProductFromJSObjectValue decodedJSON originalJSON =
    Product {
                --product_name = normalizeJSONText decodedJSON "product_name",
                product_name = getResultStringValue $ Text.JSON.valFromObj "product_name" decodedJSON,
                product_manufacturer = normalizeJSONText decodedJSON "manufacturer",
                product_family = normalizeJSONText decodedJSON "family",
                product_model = normalizeJSONText decodedJSON "model",
                -- product_announced_date = normalizeJSONText decodedJSON "announced-date",
                product_originalJSON = fst originalJSON,
                product_index = snd originalJSON
            }


--------------------------------------------

-- From the list of products, return a set of all the manufacturers
getProductManufacturers :: [Product] -> DS.Set String
getProductManufacturers products =
    DS.fromList $ map (concat . product_manufacturer) products

















