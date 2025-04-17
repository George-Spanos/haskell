module CryptoTest (main) where

import CryptoChannels as QC (run)

main :: IO ()
main = QC.run