-----------------------------------------------------------------------------
--
-- Module      :  FinalMatching
-- Copyright   :  Copyright (C) 2012 Chris A. Mennie
-- License     :  Released under the GPL version 3 license. See the included LICENSE.
--
-- Maintainer  :  Chris Mennie (chris at chrismennie.ca)
--
-- Sortable-Challenge -- An answer to the Sortable coding challenge
-- Written by Chris Mennie (chris at chrismennie.ca)
--
-----------------------------------------------------------------------------

module FinalMatching (
    bestMatchedListingsPerProduct
) where

import Matching
import Listing
import Product
import qualified Data.Map as DM
import Data.Maybe


type ListingToBestProductMap = DM.Map Int (Listing, Product, Float)
type ProductListingsMap = DM.Map Int (Product, [(Listing, Float)])


-- Given matching results where listings are not unique per product,
-- transform it into results where listings are unique to a single product
bestMatchedListingsPerProduct :: [MatchingResult] -> [MatchingResult]
bestMatchedListingsPerProduct [] = []

bestMatchedListingsPerProduct matchings =
        convertProductListingsMapToMatchingResults finalProductListingMap

    where
        -- Finish off map of products to list of best matching listings
        finalProductListingMap = fillProductListingMap emptyProductListingMap listingToProductList

        -- Initial map of all products without matching listings
        emptyProductListingMap = getEmptyProductListingMap matchings

        -- Map of listings to best match product (inverse of what we ultimately want)
        listingToProductList =
            DM.elems . listingsToBestProductMap $ flattenResults matchings :: [(Listing, Product, Float)]


-- Transform/flatten map of products -> best listings to list of MatchingResults
convertProductListingsMapToMatchingResults :: ProductListingsMap -> [MatchingResult]
convertProductListingsMapToMatchingResults plMap =
        map convertPLSToMR (DM.elems plMap)

    where
        convertPLSToMR (prod, scoredListings) =
            MatchingResult { mr_product = prod, mr_listings = scoredListings }


-- Go through listing -> product map and add each listing to the respective product in the 
-- product -> listing map
fillProductListingMap :: ProductListingsMap -> [(Listing, Product, Float)] -> ProductListingsMap
fillProductListingMap plMap listingToProductList =
        foldr addListingToProductInMap plMap listingToProductList

    where
        addListingToProductInMap (listing, prod, score) accMap =
                DM.insert (product_index prod) updatedTuple accMap
            where
                updatedTuple = (oldProduct, scoredListings ++ [(listing, score)])
                (oldProduct, scoredListings) = fromJust $ DM.lookup (product_index prod) accMap


-- Create initial map of all products without matching listings
getEmptyProductListingMap :: [MatchingResult] -> ProductListingsMap
getEmptyProductListingMap matchings =
    foldr (\mr plMap -> DM.insert (product_index $ mr_product mr)
                               (mr_product mr, [])
                               plMap)
        DM.empty matchings


-- Transform MatchingResult into convenient tuples
-- We have a many-many mapping of products/listings here
flattenResults :: [MatchingResult] -> [(Listing, Product, Float)]
flattenResults results =
        flattenMRs results

    where
        flattenMRs mrs =
            concat $
            map (\mr -> flattenSingleMR (mr_product mr) (mr_listings mr)) mrs :: [(Listing, Product, Float)]

        flattenSingleMR prod scoredListings =
            map (\l -> (fst l, prod, snd l)) scoredListings :: [(Listing, Product, Float)]


-- Create a map of best product for each listing
listingsToBestProductMap :: [(Listing, Product, Float)] -> ListingToBestProductMap
listingsToBestProductMap resultList = foldr reduceMRsToLPMap DM.empty resultList


-- Fold helper for listingsToBestProductMap
-- Keep updating listing->product map with better matches
reduceMRsToLPMap :: (Listing, Product, Float) -> ListingToBestProductMap -> ListingToBestProductMap
reduceMRsToLPMap tuple@(listing, _, _) lpMap =
        DM.insertWith bestListing (listing_index listing) tuple lpMap

    where
        bestListing l1@(_, _, s1) l2@(_, _, s2) =
            if s1 > s2 then l1 else l2

















