-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Copyright (C) 2012 Chris A. Mennie
-- License     :  Released under the GPL version 3 license. See the included LICENSE.
--
-- Maintainer  :  Chris Mennie (chris at chrismennie.ca)
--
-- Sortable-Challenge -- An answer to the Sortable coding challenge
-- Written by Chris Mennie (chris at chrismennie.ca)
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import System.Environment
import Listing
import Product
import Matching
import FinalMatching
import System.CPUTime
import Text.Printf
import Data.List
import GHC.Conc (numCapabilities)


main :: IO ()
main = do
    startTime <- getCPUTime
    progName <- getProgName

    (listingsFile, productsFile, resultsFile) <- getArguments
    if listingsFile /= ""
        then do
            -- Do the work
            processData listingsFile productsFile resultsFile numThreads

            -- Output timing information
            endTime <- getCPUTime
            let diff = (fromIntegral (endTime - startTime)) / (tenToTwelve) :: Double
            printf "Computation time: %0.3f sec\n" (diff :: Double)
        else putStrLn $ "Usage: " ++ progName ++ " <listings.txt> <products.txt> <results.json>"

    where
        numThreads = GHC.Conc.numCapabilities
        tenToTwelve = 1000000000000 :: Double


-- Gather command line arguments, if any given, or provide reasonable defaults
getArguments :: IO (String, String, String)
getArguments = do
    args <- getArgs

    case (length args) of
        0 -> return ("listings.txt", "products.txt", "results.json")
        3 ->
            do
                let listingsFile = (args !! 0)
                let productsFile = (args !! 1)
                let resultsFile = (args !! 2)
                return (listingsFile, productsFile, resultsFile)
        _ -> return ("", "", "")


-- Read the data files, process them, and write out the results
processData :: String -> String -> String -> Int -> IO ()
processData listingFile productsFile resultsFile numThreads = do
    -- Load the input files
    listingFileContents <- readFile listingFile
    productFileContents <- readFile productsFile

    let
        products = readProducts productFileContents
        productManufacturers = getProductManufacturers products
        listings = readListings listingFileContents productManufacturers

    -- Do the actual work and write results
    writeResults resultsFile $
        bestMatchedListingsPerProduct $
        listingsForProducts listings products 1 --numThreads

    putStrLn "Finished processing."


-- Output the results
writeResults :: String -> [MatchingResult] -> IO ()
writeResults resultsFile matchingResults  =
    writeFile resultsFile $ linedItems matchingResults


-- Join a list of entities into a single string, delimited by new lines
linedItems :: Show a => [a] -> String
linedItems items =
    intercalate "\n" shownItems
    where
        shownItems = map (\i -> show i) items












