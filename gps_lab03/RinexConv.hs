-- 2025-11-18

{- | Replace the letter 'D' with 'E' in the data section of RINEX 3.04 file
     so that scientific notation uses 'E' instead of Fortran-style 'D'.
     The header remains unchanged.

     NOTE:
       It is important to detect END OF HEADER from column 60,
       because there can be END OF HEADER in comment fields.

     Input:
       - source RINEX file name      (set in the code)
       - destination RINEX file name (set in the code)

     Output: 
       - destination RINEX file with normalized scentific notation with letter 'E'
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as L8    
import Data.Char                                  (isSpace)
import Data.Int                                   (Int64)

main :: IO ()
main = do
  let
      sn = "source.nav"                                      -- Input: source file name     
      dn = "destination.nav"                                 -- Input: destination file name

  putStrLn "Start processing"
  convertRinex sn dn
  putStrLn "Processing complete."
           
-- | Convert a RINEX file by:
--   * separating the header (up to the line containing "END OF HEADER"),
--   * copying the header unchanged,
--   * replacing all 'D' or 'd' with 'E' in the data section,
--   * writing the result to the destination file.
convertRinex
    :: FilePath                                              -- ^ source file name
    -> FilePath                                              -- ^ destination file name
    -> IO ()
convertRinex sn dn = do
    bs <- L8.readFile sn
    let ls     = L8.lines bs                                 -- Only header lines will be read
        hdrLen = headerLength ls                             
    case ls of
      []     -> error "Empty file"
      (l1:_) -> do
              let rinexVer = trim $ getField 0 9 l1
              if rinexVer == "3.04"
              then do
                let hdr     = L8.take hdrLen bs
                    dataSec = L8.drop hdrLen bs
                L8.writeFile dn (hdr <> (L8.map replaceD dataSec))
              else error "This is not RINEX 3.04 file"

-- | Compute RINEX header length including END OF HEADER line
headerLength :: [L8.ByteString] -> Int64
headerLength ls = case break isEndOfHeader ls of
                    (_   , []  ) -> error "Cannot detect rinex header."
                    (part, l1:_) -> sum (map L8.length part) + L8.length l1
    where
      isEndOfHeader line = trim (L8.drop 60 line) == L8.pack "END OF HEADER"

-- | Replace 'D' or 'd' with 'E'.
--   In RINEX, floating-point numbers may use Fortran-style
--   scientific notation with 'D'. This function normalizes
--   them to the standard 'E' notation.
replaceD :: Char -> Char
replaceD c
  | c == 'D'  = 'E'
  | c == 'd'  = 'E'
  | otherwise = c

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

-- | Extract a substring from a line:
--   starting at position 'start' (0-based),
--   with length 'len'.
--   Used to read fixed-width fields.       
getField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
getField start len = L8.take len . L8.drop start       

