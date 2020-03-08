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
import System.Posix.Signals (Handler, Handler(CatchOnce), Handler(Catch), installHandler, sigINT, sigTERM)
import Data.List

data Options = Options {
    frequency  :: Double
}

optParser :: Parser Options
optParser = Options 
    <$> option auto (
        long "frequency"  
        <> short 'f' 
        <> metavar "FREQUENCY" 
        <> help "Frequency to tune to, Hz"
        <> showDefault
        <> value 4e6
        )

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Receive and demodulate broadcast FM radio" <> header "RTLSDR FM")

handler dev stream = do
    _ <- soapySDRDeviceDeactivateStream dev stream 0 0
    _ <- soapySDRDeviceCloseStream dev stream
    unmakeResult <- soapySDRDeviceUnmake dev
    putStrLn ("unmake result:" ++ show unmakeResult)

transmitToneTest Options{..} = do
    dev <- soapySDRDeviceMakeStrArgs "driver=hackrf"
    -- from hackrf_transfer help : # Sample rate in Hz (4/8/10/12.5/16/20MHz, default 10MHz)
    -- with smaller sample rates like 100k or 300k HackRF seems to transmit just a carrier
    r1 <- soapySDRDeviceSetSampleRate dev DirectionTX Channel0 4.0e6
    print r1
    sr <- soapySDRDeviceGetSampleRate dev DirectionTX Channel0
    print ("samplerate:" ++ show sr)
    r2 <-soapySDRDeviceSetFrequency dev DirectionTX Channel0 (CDouble frequency) (SoapySDRKwargs nullPtr)
    print r2
    z1 <- soapySDRDeviceSetupStream dev DirectionTX "CF32"  0 0 (SoapySDRKwargs nullPtr)
    let stream = snd z1
    r3 <- soapySDRDeviceActivateStream dev stream 0 0 0
    print r3

    installHandler sigINT (Catch (handler dev stream)) Nothing
    installHandler sigTERM (Catch (handler dev stream)) Nothing

    let num_samples = 131072

    let t = [0.0..(fromIntegral num_samples)]
    let f = map (\i -> sin(i/5000)) t
    let i = zipWith (\i j -> CFloat(sin(i/50+j))) t f
    let q = zipWith (\i j -> CFloat(cos(i/50-j))) t f
    let iq = concat (transpose [i, q])

    array <- allocaBytes (num_samples * 8) $ \buf -> do
        flags <- malloc :: IO (Ptr CInt)
        with buf $ \bufptr -> do
            pokeArray buf iq
            forever $ do
                elementsW <- writeStream dev stream bufptr (fromIntegral num_samples :: CULong) flags 0 1000000
                print elementsW                
    print "done"

main = do
    op <- execParser opt
    transmitToneTest op