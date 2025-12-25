-- 2025-12-25

{- | The program Creates copy of a RINEX 3.04 file, replacing the letter
     'D' with 'e' in the data section so that scientific notation uses
     'e' instead of Fortran-style 'D'. The header remains unchanged.

     NOTE:
       It is important to detect END OF HEADER from column 60,
       because there can be END OF HEADER in comment fields.

     Input:
       - source RINEX file name      (to set in the code)   sn
       - destination RINEX file name (to set in the code)   dn

     Output: 
       - destination RINEX file with normalized scentific notation with letter 'e'
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as L8    
import           Data.Char                           (isSpace)
import           Data.Int                            (Int64)
import           Text.Printf

main :: IO ()
main = do
  let
      sn = "source.nav"                                     -- Input: source file name     
      dn = "destination.nav"                                -- Input: destination file name

  printf "Start processing\n"
  convert sn dn
  printf "Processing complete\n"
           
-- | Convert a RINEX file
convert
    :: FilePath                                             -- ^ source file name
    -> FilePath                                             -- ^ destination file name
    -> IO ()
convert sn dn = do
    bs <- L8.readFile sn
    let bs' = convertNotation bs
    L8.writeFile dn bs'

-- | Convert scientific notation by:
--   * separating the header (up to the line containing "END OF HEADER"),
--   * copying the header unchanged,
--   * replacing all 'D' or 'd' with 'E' in the data section,      
convertNotation :: L8.ByteString -> L8.ByteString                   
convertNotation bs
    | L8.null bs          = error "Empty file"
    | rnxVer /= "3.04"  = error "Not RINEX 3.04 file"
    | otherwise =  
        let pieces = L8.split '\n' bs                       -- Only header pieces will be read
                                                            -- Don't use L8.lines because it works differently
            hdrLen = headerLength pieces
            hdr     = L8.take hdrLen bs
            dataSec = L8.drop hdrLen bs
        in hdr <> (L8.map replaceD dataSec)
    where
       rnxVer = trim $ takeField 0 9 bs
        
-- | Compute RINEX header length including END OF HEADER line
headerLength :: [L8.ByteString] -> Int64
headerLength ls =
    case break isEndOfHeader ls of
      (_   , []  )  -> error "Cannot detect rinex header."
      (part, l1:[]) -> sum (map ((+1) . L8.length) part) +  L8.length l1      -- +1 to count '\n'
      (part, l1:_)  -> sum (map ((+1) . L8.length) part) + (L8.length l1 + 1) -- +1 to count '\n'
    where
      isEndOfHeader line = trim (L8.drop 60 line) == L8.pack "END OF HEADER"

-- | Replace 'D' or 'd' with 'e'.
--   In RINEX, floating-point numbers may use Fortran-style
--   scientific notation with 'D'. This function normalizes
--   them to the standard 'e' notation.
replaceD :: Char -> Char
replaceD c
  | c == 'D'  = 'e'
  | c == 'd'  = 'e'
  | otherwise = c

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

-- | Extract a substring from a line:
--   starting at position 'start' (0-based),
--   with length 'len'.
--   Used to read fixed-width fields.       
takeField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start       

