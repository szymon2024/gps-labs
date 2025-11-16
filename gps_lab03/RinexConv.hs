-- 2025-11-16

{- | Replace the letter 'D' with 'E' in the data section of RINEX 3.04 file
     so that scientific notation uses 'E' instead of Fortran-style 'D'.
     The header remains unchanged.

     NOTE:
       It is important to detect END OF HEADER from column 60,
       because someone can enter END OF HEADER in comment fields.

     Input:
       - source RINEX file name      (set in the code)
       - destination RINEX file name (set in the code)

     Output: 
       - destination RINEX file with normalized scentific notation with letter 'E'
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as L8    
import Data.Char (isSpace)
import Data.Int (Int64)

main :: IO ()
main = do
  let
      sn = "source.nav"                                      -- Input: source file name     
      dn = "destination.nav"                                 -- Input: destination file name
           
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
    let (hdr, rest) = separateHeader bs
    case hdr of                      
      []     -> error "Cannot detect rinex header."
      (l1:_) ->
          let rinexVer = trim $ getField 0 9 l1             -- rinex version
          in case rinexVer of
               "3.04"    -> L8.writeFile dn $ L8.concat hdr <> L8.map replaceD rest
               otherwise ->  error $ "RINEX version " ++ L8.unpack rinexVer
                                  ++ " found. Expected version 3.04."

-- | Separate the header from the data section.
--   The header is assumed to end with a line containing the label
--   "END OF HEADER" in columns 61–80 (as per RINEX specification).
--   Returns a tuple: (list of header lines, remainder of the file).
separateHeader :: L8.ByteString -> ([L8.ByteString], L8.ByteString)
separateHeader bs = go [] bs
    where
      l = detectLineLength bs
      go acc rest
          | L8.null rest = ([], rest)
          | otherwise    =
              let
                  (line, rest') = L8.splitAt l rest
                  acc'          = line : acc
                  label         = trim $ getField 60 20 line
              in if label == "END OF HEADER"
                 then (reverse acc', rest')
                 else go acc' rest'

-- | Detect the line length of the RINEX file:
--   * 81 characters (80 + LF)
--   * 82 characters (80 + CRLF)
--   Throws an error if no valid line ending is detected.
detectLineLength :: L8.ByteString -> Int64
detectLineLength bs
    | bs L8.!? 81 == Just '\n' = 82
    | bs L8.!? 80 == Just '\n' = 81
    | otherwise                = error "Cannot detect end of line"

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

