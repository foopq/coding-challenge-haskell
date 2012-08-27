-----------------------------------------------------------------------------
--
-- Module      :  Matching
-- Copyright   :  Copyright (C) 2012 Chris A. Mennie
-- License     :  Released under the GPL version 3 license. See the included LICENSE.
--
-- Maintainer  :  Chris Mennie (chris at chrismennie.ca)
--
-- Sortable-Challenge -- An answer to the Sortable coding challenge
-- Written by Chris Mennie (chris at chrismennie.ca)
--
-----------------------------------------------------------------------------

module Matching (
    MatchingResult (MatchingResult),
    mr_product,
    mr_listings,
    listingsForProducts
) where


import qualified Listing as L
import qualified Product as P
import Data.Maybe
import Data.List (genericLength, intercalate)
import Data.List.Split (splitEvery)
import Control.Parallel.Strategies
import Control.DeepSeq

data FilterMode = Manufacturer | Model | Family deriving (Eq, Show)


data MatchingResult = MatchingResult {
                                            mr_product :: P.Product,
                                            mr_listings :: [(L.Listing, Float)]
                                        }

instance NFData MatchingResult where
    rnf (MatchingResult p ls) = rnf p `seq` rnf ls


-- For debug output
{-
instance Show MatchingResult where
    show (MatchingResult p ls) =
        "MatchingResult: \n" ++
        (show p) ++ "\n" ++
        "[\n" ++ (unlines (map showListing ls)) ++ "]\n"
        where
            showListing :: (L.Listing, Float) -> String
            showListing (listing, score) = "Listing score: " ++ (show score) ++ "\n" ++
                                            (show listing) ++ "\n" :: String
-}

instance Show MatchingResult where
    show (MatchingResult p ls) =
            "{\"product_name\":\"" ++ (P.product_name p) ++ "\", " ++
            "\"listings\":[" ++ jsonedListings ++ "]}"
        where
            jsonedListings = intercalate ", " originalListingJSONs
            originalListingJSONs = map (\(l, _) -> (L.listing_originalJSON l)) ls


data MatchInfo = MatchInfo {
                                substringMatchAmount :: Float,
                                matchedPosition :: Int,
                                diffPositionFromOriginal :: Int,
                                matchedPairDistanceDelta :: Int,
                                positionalMatchBonus :: Float,
                                isMatched :: Bool
                             } deriving (Show)


defaultMatchInfo :: MatchInfo
defaultMatchInfo = MatchInfo {
                                substringMatchAmount = 0.0,
                                matchedPosition = 0,
                                diffPositionFromOriginal = 0,
                                matchedPairDistanceDelta = 0,
                                positionalMatchBonus = 0.0,
                                isMatched = False
                              }



-- For each product, return a list of listings that could match with that product (and then return a list of all these)
listingsForProducts :: [L.Listing] -> [P.Product] -> Int -> [MatchingResult]
listingsForProducts listings products numThreads
    | numThreads == 1   = map (listingsForProduct listings) products
    | otherwise         = concat $ map (parallelListingsForProducts listings) (splitEvery numThreads products)


-- Compute the overall result of listingsForProducts using threads
parallelListingsForProducts :: [L.Listing] -> [P.Product] -> [MatchingResult]
parallelListingsForProducts listings products =
    parMap rdeepseq (listingsForProduct listings) products


-- For given product, determine which listings could match with it
listingsForProduct :: [L.Listing] -> P.Product -> MatchingResult
listingsForProduct listings curProduct =
        MatchingResult { mr_product = curProduct, mr_listings = filteredListings }
    where
        filteredListings =
            filterByFinalScore (P.product_manufacturer curProduct) $
            filterByFamily (P.product_family curProduct) $
            filterByModel (P.product_model curProduct) $
            filterByManufacturer (P.product_manufacturer curProduct) $
            (map (\l -> (l, 0.0)) listings)


-- Filter out listings if they don't meet our threshold
filterByFinalScore :: [String] -> [(L.Listing, Float)] -> [(L.Listing, Float)]
filterByFinalScore manufacturer listings =
        filter (\(_, score) -> score > minScore) listings
    where
        minScore = 
            if elem "canon" manufacturer
                then 0.899
                else 0.7


-- Filter out listings based on matching manufacturer
filterByManufacturer :: [String] -> [(L.Listing, Float)] -> [(L.Listing, Float)]
filterByManufacturer manufacturerList listings =
        commonFilter listings manufacturerList manufacturerWeight L.listing_manufacturer Manufacturer
    where
        manufacturerWeight = 0.25


-- Filter out listings based on matching model
filterByModel :: [String] -> [(L.Listing, Float)] -> [(L.Listing, Float)]
filterByModel modelList listings =
        commonFilter listings modelList modelWeight L.listing_title Model
    where
        modelWeight = 0.55


-- Filter out listings based on matching family
filterByFamily :: [String] -> [(L.Listing, Float)] -> [(L.Listing, Float)]
filterByFamily familyList listings =
        commonFilter listings familyList familyWeight L.listing_title Family
    where
        familyWeight = 0.20


-- Common filtering function.. given lists of words to match, filter out listings that don't meet base threshold
commonFilter :: [(L.Listing, Float)] -> [String] -> Float -> (L.Listing -> [String]) ->
                    FilterMode -> [(L.Listing, Float)]
commonFilter listings categoryStr categoryWeight listingExtractionMethod filterMode =
    mapMaybe (commonFilterMapHelper categoryStr categoryWeight listingExtractionMethod filterMode) listings


-- Real work for commonFilter. Only looking at manufacturer will actually early filter out listings.
commonFilterMapHelper :: [String] -> Float -> (L.Listing -> [String]) ->
                            FilterMode -> (L.Listing, Float) -> Maybe (L.Listing, Float)
commonFilterMapHelper categoryStr categoryWeight listingExtractionMethod filterMode listing =
        case filterMode of
            Manufacturer -> if normalizedWeight > 0.0
                                then return (fst listing, normalizedWeight)
                                else Nothing
            _ -> return (fst listing, normalizedWeight)

    where
        normalizedWeight = baseWeight * categoryWeight + snd listing  -- normalized weight for filter plus previous score
        baseWeight = computeBaseWeight categoryStr filterMode listingData
        listingData = listingExtractionMethod $ fst listing  -- get listing data using accessor function passed in


-- For the set of words in the product category we're looking at, compare them
-- to the extracted words of the listing and return a normalized score
computeBaseWeight :: [String] -> FilterMode -> [String] -> Float
computeBaseWeight categoryStr filterMode listingData =
        finalScore

    where
        finalScore = scoreWithUnmatchedListingPenalty

        -- Final score has severe penalty for unmatched manufacturer words (when looking at manufacturer)
        scoreWithUnmatchedListingPenalty =
            if filterMode == Manufacturer
                then scoreWithUnmatchedProductWordPenalty - 0.1 * (genericLength listingData - genericLength matchInfosWithPosMatchBonuses)
                else scoreWithUnmatchedProductWordPenalty

        -- 20% penalty for each product word that wasn't matched
        scoreWithUnmatchedProductWordPenalty = normalizedPartialScore - 0.20 * (genericLength categoryStr - genericLength matchInfosWithPosMatchBonuses)

        -- normalize lower end of score to 0
        normalizedPartialScore = if maxScore > 0.0 then scoreWithUnmatchedModelPenalties / maxScore else 0.0

        -- determine the maximum possible score (for normalization)
        maxScore = (50.0 + 25.0) * (genericLength matchInfos) + partialMaxScore

        -- Compute the weighted partial harmonic number for the exact matching weight portion (fixed values to use, irrespective of words)
        (partialMaxScore, matchInfosWithPosMatchBonuses) = computePositionalMatchBonuses matchInfos filterMode :: (Float, [MatchInfo])

        -- For each word in the product category string, compute the scoring metrics
        matchInfos = fillMatchInfos categoryStr listingData filterMode :: [MatchInfo]

        -- Incur penalties for model words whose position wasn't matched exactly
        scoreWithUnmatchedModelPenalties =
            if filterMode == Model
                then foldr (\mi score -> score - positionalMatchBonus mi) initialScore matchInfosWithPosMatchBonuses
                else initialScore

        -- Compute initial score of matching word based on distance delta of match and amount of word matched
        initialScore = foldr (addInitialScore filterMode) 0.0 matchInfosWithPosMatchBonuses


-- For each word in the product category string, compute the scoring metrics
fillMatchInfos :: [String] -> [String] -> FilterMode -> [MatchInfo]
fillMatchInfos categoryStr listingData filterMode =
        filter (\mi -> isMatched mi == True) finalCategoryStrMatchInfos

    where
        indexedCategoryStr = zip categoryStr [0..] :: [(String, Int)]
        indexedListingData = zip listingData [0..] :: [(String, Int)]

        -- Compute the distance deltas of all the MatchInfos
        finalCategoryStrMatchInfos = computeMatchedPairDistanceDeltas categoryStrMatchInfos

        -- Compute the initial MatchInfos
        -- n^2 because we end up considering all word pairs (to try to find best matches)
        categoryStrMatchInfos =
            (flip map) indexedCategoryStr
                (\productWord -> determineMatchInfoForProductWord productWord filterMode indexedListingData)


-- Encapsulate the matching distance between adjacent words in original word list
-- We're looking at pairs of adjacent words/MatchInfos
computeMatchedPairDistanceDeltas :: [MatchInfo] -> [MatchInfo]
computeMatchedPairDistanceDeltas [] = [] -- I don't think we should ever get here
computeMatchedPairDistanceDeltas matchInfos
    | length matchInfos == 1 = matchInfos

    | otherwise =
            matchInfosWithDistanceDeltas ++ [last matchInfos]

        where
            matchInfosWithDistanceDeltas = zipWith getDistanceDelta matchInfos $ tail matchInfos

            getDistanceDelta m1 m2 = 
                                    m1 { matchedPairDistanceDelta = abs $ matchedPosition m2 - matchedPosition m1 }


-- Determine the best matching for product/listing word pair
determineMatchInfoForProductWord :: (String, Int) -> FilterMode -> [(String, Int)] -> MatchInfo
determineMatchInfoForProductWord productWord filterMode indexedListingData =
        determineBestMatchInfo matchedMatchInfos filterMode
    where
        matchedMatchInfos = catMaybes maybeMatches :: [MatchInfo]
        maybeMatches = (flip map) indexedListingData (determineMatchInfoHelper productWord filterMode)  :: [Maybe MatchInfo]


-- Go through MatchInfos and pick out the best one (based on position and substring matching amount)
determineBestMatchInfo :: [MatchInfo] -> FilterMode -> MatchInfo
determineBestMatchInfo [] filterMode = determineBestMatchInfo [defaultMatchInfo] filterMode
determineBestMatchInfo justMatchInfos Manufacturer = head justMatchInfos
determineBestMatchInfo justMatchInfos _ =
        foldr1 compareMatchInfos justMatchInfos
    where
        compareMatchInfos mi1 mi2 =
            if diffPositionFromOriginal mi1 /= diffPositionFromOriginal mi2
                then if diffPositionFromOriginal mi1 < diffPositionFromOriginal mi2
                            then mi1
                            else mi2
                else if substringMatchAmount mi1 < substringMatchAmount mi2
                            then mi1
                            else mi2


-- Create initial MatchInfo for product/listing word pair
determineMatchInfoHelper :: (String, Int) -> FilterMode -> (String, Int) -> Maybe MatchInfo
determineMatchInfoHelper productWordPair filterMode listingWordPair
    | length listingWord < length productWord =
        Nothing

    | otherwise =
            if ((filterMode == Manufacturer) && (partialMatch == True)) || (productWord == listingWord)
                then return MatchInfo {
                                    substringMatchAmount = genericLength productWord / genericLength listingWord,
                                    matchedPosition = listingWordIndex,
                                    diffPositionFromOriginal = abs listingWordIndex - productWordIndex,
                                    matchedPairDistanceDelta = 0,
                                    positionalMatchBonus = 0.0,
                                    isMatched = True
                                  }
                else Nothing

        where
            productWord = fst productWordPair
            productWordIndex = snd productWordPair

            listingWord = fst listingWordPair
            listingWordIndex = snd listingWordPair

            partialMatch = (listingWord == take (length listingWord) productWord) ||
                           (reverse listingWord == take (length listingWord) (reverse productWord))


-- Depending on filtering mode, compute bonuses for exact positional matching of words
computePositionalMatchBonuses :: [MatchInfo] -> FilterMode -> (Float, [MatchInfo])
computePositionalMatchBonuses [] _ = (0.0, [])

-- Compute the weighted partial harmonic number for the exact matching weight portion
-- XXX: This could probably be handled with a look-up table
computePositionalMatchBonuses matchInfos Manufacturer =
        (maxScore, matchInfosWithPosMatchBonuses)

    where
        harmonicValues = map (\x -> 1.0 / x) [1.0..(genericLength matchInfos)] :: [Float]
        maxScore = 50.0 * sum harmonicValues :: Float
        matchInfosWithPosMatchBonuses =
            map (\mi -> mi { positionalMatchBonus = 50.0 * (1.0 / fromIntegral (matchedPosition mi + 1)) }) matchInfos

-- Compute the weighted "balanced" partial harmonic number for the exact matching weight portion
computePositionalMatchBonuses matchInfos Model =
        (maxScore, matchInfosWithPosMatchBonuses)

    where
        maxScore = balancedMaxScore $ length matchInfos
        matchInfosWithPosMatchBonuses =
            map (\mi -> mi { positionalMatchBonus = 50.0 * balancedHarmonic (matchedPosition mi) (length matchInfos)} ) matchInfos :: [MatchInfo]

-- No matching bonuses for family
computePositionalMatchBonuses matchInfos _ = (0.0, matchInfos)


-- Helper for computePositionalMatchBonuses
balancedMaxScore :: Int -> Float
balancedMaxScore listLength =
        if isEvenListLength
            then halfsum * 2.0
            else halfsum * 2.0 + 1.0 / fromIntegral (halfpoint + 1)

    where
        isEvenListLength = listLength `mod` 2 == 0 :: Bool
        halfpoint = listLength `div` 2 :: Int
        halfsum = foldr (\val acc -> acc + 1.0 / fromIntegral val ) 0 [1..halfpoint]


-- Helper for computePositionalMatchBonuses
balancedHarmonic :: Int -> Int -> Float
balancedHarmonic position listLength =
        if position >= midpoint
            then 1.0 / fromIntegral (reflectedPosition + 1)
            else 1.0 / fromIntegral (position + 1)

    where
        isEvenListLength = listLength `mod` 2 == 0 :: Bool
        midpoint = (listLength - 1) `div` 2 + 1 :: Int
        reflectedPosition =
            if isEvenListLength
                then (midpoint - 1) - (position `mod` midpoint)
                else (midpoint - 1) - ((position + 1) `mod` midpoint)


------------------------------------------------

-- Compute initial score of matching word based on distance delta of match and amount of word matched
-- curScore is accumulator
addInitialScore :: FilterMode -> MatchInfo -> Float -> Float
addInitialScore filterMode matchInfo curScore =
        curScore + 50.0 * substringMatchAmount matchInfo
            + 25.0 * (1.0 / fromIntegral (matchedPairDistanceDelta matchInfo + 1))
            + miPositionalMatchBonus filterMode

    where
        -- Matching words for manufacturer are more important the closer to the expected word
        miPositionalMatchBonus Manufacturer =
            positionalMatchBonus matchInfo * (1.0 / fromIntegral (diffPositionFromOriginal matchInfo + 1))
        miPositionalMatchBonus _ = positionalMatchBonus matchInfo

















