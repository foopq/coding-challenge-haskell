-----------------------------------------------------------------------------
--
-- Module      :  Listing
-- Copyright   :  Copyright (C) 2012 Chris A. Mennie
-- License     :  Released under the GPL version 3 license. See the included LICENSE.
--
-- Maintainer  :  Chris Mennie (chris at chrismennie.ca)
--
-- Sortable-Challenge -- An answer to the Sortable coding challenge
-- Written by Chris Mennie (chris at chrismennie.ca)
--
-----------------------------------------------------------------------------

module Listing (
    Listing,
    listing_title,
    listing_manufacturer,
    listing_originalJSON,
    listing_index,
    readListings
) where

import Text.JSON
import Data.Either
import qualified Data.Set as DS
import InputDataCommon
import Control.DeepSeq


data Listing = Listing {
                    listing_title :: [String],
                    listing_manufacturer :: [String],
                    -- listing_currency :: [String],
                    -- listing_price :: [String],
                    listing_originalJSON :: String,
                    listing_index :: Int
                } deriving (Show)


instance NFData Listing where
    rnf (Listing t m o i) = rnf t `seq` rnf m `seq` rnf o `seq` rnf i


--instance Show Listing where
--    show (Listing t m o i) = "[Listing " ++ o ++ "]"


-- Given listings.txt as a string and set of manufacturers, parse the listings into a list of Listing
readListings :: String -> DS.Set String -> [Listing]
readListings listingFileContents productManufacturers =
        unwrapResultedListings resultedListings
    where
        resultedListings = map (readSingleListing productManufacturers) indexedFileLines
        indexedFileLines = zipWith (\l i -> (l, i)) fileLines [1..]
        fileLines = DS.toList . DS.fromList . lines $ listingFileContents     -- use set to remove duplicate lines


-- Convert our Listing list into one that's escaped from the Result monad
unwrapResultedListings :: [Result Listing] -> [Listing]
unwrapResultedListings resultedListings =
        rights eitherListings
    where
        eitherListings = map resultToEither resultedListings


-- Parse the JSON into a Listing
readSingleListing :: DS.Set String -> (String, Int) -> Result Listing
readSingleListing productManufacturers jsonLine = do
    jsonObject <- decodeJsonLine $ fst jsonLine
    let listing = getListingFromJSObjectValue jsonObject jsonLine

    -- possibly filter out some listings
    if isOkListing listing productManufacturers
        then return listing
        else Error ""

-- Convert the decoded JSON object into a Listing
getListingFromJSObjectValue :: JSObject JSValue -> (String, Int) -> Listing
getListingFromJSObjectValue decodedJSON originalJSON =
    Listing {
                listing_title = normalizeJSONText decodedJSON "title",
                listing_manufacturer =  normalizeJSONText decodedJSON "manufacturer",
                -- listing_currency = normalizeJSONText decodedJSON "currency",
                -- listing_price = normalizeJSONText decodedJSON "price",
                listing_originalJSON = fst originalJSON,
                listing_index = snd originalJSON
            }


-- Blacklist listings whose titles contain the for "for" or whose manufacturer
-- (first word or first+second word of) does not match a manufacturer seen in the product
-- list
isOkListing :: Listing -> DS.Set String -> Bool
isOkListing listing productManufacturers =
    case isOkListingHelper listing productManufacturers of
        Just _ -> True
        Nothing -> False


-- Helper for isOkListing. Short curcuits if a condition doesn't match due to use
-- of Maybe monad
isOkListingHelper :: Listing -> DS.Set String -> Maybe Listing
isOkListingHelper listing productManufacturers =
    checkForFor listing >>= checkForManufacturerMatch productManufacturers


-- If the title of the listing contains the word "for", we don't want it.
-- This is very English focused
checkForFor :: Listing -> Maybe Listing
checkForFor listing =
    if "for" `notElem` (listing_title listing)
        then return listing
        else Nothing


-- If the listing's manufacturer isn't one of the product manufacturer, we don't want it.
-- We check the first word and concatenated first and second words.
checkForManufacturerMatch :: DS.Set String -> Listing -> Maybe Listing
checkForManufacturerMatch productManufacturers listing =
    if (listingManufacturerOneStr `DS.member` productManufacturers) ||
       (listingManufacturerTwoStr `DS.member` productManufacturers)
        then return listing
        else Nothing

    where
        listingManufacturerTwoStr = concat . (take 2) $ listing_manufacturer listing
        listingManufacturerOneStr = concat . (take 1) $ listing_manufacturer listing

















