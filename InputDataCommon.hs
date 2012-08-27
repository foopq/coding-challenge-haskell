-----------------------------------------------------------------------------
--
-- Module      :  InputDataCommon
-- Copyright   :  Copyright (C) 2012 Chris A. Mennie
-- License     :  Released under the GPL version 3 license. See the included LICENSE.
--
-- Maintainer  :  Chris Mennie (chris at chrismennie.ca)
--
-- Sortable-Challenge -- An answer to the Sortable coding challenge
-- Written by Chris Mennie (chris at chrismennie.ca)
--
-----------------------------------------------------------------------------

module InputDataCommon (
    decodeJsonLine,
    normalizeJSONText,
    getResultStringValue
) where

import Text.JSON
import Data.Char (toLower, isAlphaNum)


-- Decodes string into JSON object
decodeJsonLine :: String -> Text.JSON.Result (JSObject JSValue)
decodeJsonLine jsonLine = Text.JSON.decode jsonLine


-- Grab an attribute from a JSON object, break it into words, filtering out words we
-- don't care about and normalizing the rest.
normalizeJSONText :: JSObject JSValue -> String -> [String]
normalizeJSONText jsonObject attribute =
        -- Return non-empty words
        filter (not . null) normalizedStringWords

    where
        -- Filter out words that are useless noise and trim whitespace from the rest
        normalizedStringWords = map normalizeWord baseLowerStringWords

        -- Split string into space separated words
        baseLowerStringWords = words lowerCastStrNoDashes

        -- Remove any '-' from string
        lowerCastStrNoDashes = filter (/= '-') lowerCaseStr

        -- Turn everything lower case
        lowerCaseStr = map toLower baseAttributeStr

        -- Pure string of JSON attribute
        baseAttributeStr = getResultStringValue attributeFromJson

        -- String corresponding to attribute on JSON object
        attributeFromJson = Text.JSON.valFromObj attribute jsonObject :: Result String


-- Help string escape from Result monad
getResultStringValue :: Result String -> String
getResultStringValue resultString =
    case resultString of
        Ok str -> str
        Error _ -> ""


-- If not a noise word, return trimmed version, otherwise return empty string
normalizeWord :: String -> String
normalizeWord word =
    if trimmedWord `notElem` noiseEntries
        then trimmedWord
        else ""
    where
        trimmedWord = trim word


-- trim leading and trailing whitespace from string
trim :: String -> String
trim =
        f . f
    where
        f = reverse . dropWhile (not . isAlphaNum)


-- List of words that are useless noise and should be ignored
noiseEntries :: [String]
noiseEntries = ["gmbh", "inc", "ltd", "uk", "corporation",
                "international", "llc", "co", "plc",
                "with", "and"] -- "for" should be in here, but we use it for filtering listings































