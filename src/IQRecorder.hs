{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Soapy
import System.IO
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Marshal.Utils
import Control.Monad (forever)
import Control.Monad.Trans.Except
import Options.Applicative
import Control.Error.Util

data Options = Options {
    frequency  :: Double,
    output     :: FilePath,
    samplerate :: Double
}

optParser :: Parser Options
optParser = Options 
    <$> option auto (
        long "frequency"  
        <> short 'f' 
        <> metavar "FREQUENCY" 
        <> help "Frequency to tune to"
        )
    <*> strOption (
        long "output"
        <> short 'o'
        <> metavar "FILENAME"  
        <> help "Output filename"
        )
    <*> option auto (
        long "samplerate"   
        <> short 's' 
        <> metavar "SAMPLERATE"  
        <> help "Sample rate, Hz"
        )

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Receive and demodulate broadcast FM radio" <> header "RTLSDR FM")

doIt Options{..} = do
    print frequency
    dev <- soapySDRDeviceMakeStrArgs "driver=rtlsdr"
    r1 <- soapySDRDeviceSetSampleRate dev DirectionRX Channel0 (CDouble samplerate)
    print r1
    sr <- soapySDRDeviceGetSampleRate dev DirectionRX Channel0
    print ("samplerate:" ++ show sr)
    a<-soapySDRDeviceSetGain dev DirectionRX Channel0 60.0
    r2 <-soapySDRDeviceSetFrequency dev DirectionRX Channel0 (CDouble frequency) (SoapySDRKwargs nullPtr)
    print r2
    z1 <- soapySDRDeviceSetupStream dev DirectionRX "CF32"  0 0 (SoapySDRKwargs nullPtr)
    let stream = snd z1
    r3 <- soapySDRDeviceActivateStream dev stream 0 0 0
    print r3
    let num_samples = 131072
    array <- allocaArray (num_samples * 2) $ \buf -> do
        flags <- malloc :: IO (Ptr CInt)
        timeNs <- malloc :: IO (Ptr CLLong)
        fh <- openBinaryFile output WriteMode
        forever $
            with buf $ \bufptr -> do
                elementsRead <- readStream dev stream bufptr (fromIntegral num_samples :: CInt) flags timeNs 1000000
                print elementsRead
                hPutBuf fh buf (num_samples * 2 * 4)
    print "done"

main = do
    op <- execParser opt
    doIt op