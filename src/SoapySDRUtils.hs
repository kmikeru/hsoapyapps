{-# LANGUAGE OverloadedStrings #-}

module SoapySDRUtils where

import Soapy
import System.IO
import System.Random
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Marshal.Utils
import Data.Complex
import Data.List
import Data.List.Split
import Numeric.FFTW
import GHC.Float
import GHC.Word
import Data.Coerce (coerce)

data DeviceWithStream = DeviceWithStream SoapySDRDevice SoapySDRStream

setupStream :: String -> Double -> Double -> IO DeviceWithStream
setupStream args freq bw = do
    dev <- soapySDRDeviceMakeStrArgs args -- "driver=rtlsdr"
    r1 <- soapySDRDeviceSetSampleRate dev DirectionRX Channel0 (CDouble bw)
    print r1
    sr <- soapySDRDeviceGetSampleRate dev DirectionRX Channel0
    print ("samplerate:" ++ show sr)
    a<-soapySDRDeviceSetGain dev DirectionRX Channel0 30.0
    r2 <-soapySDRDeviceSetFrequency dev DirectionRX Channel0 (CDouble freq) (SoapySDRKwargs nullPtr)
    print r2
    z1 <- soapySDRDeviceSetupStream dev DirectionRX "CF32"  0 0 (SoapySDRKwargs nullPtr)
    let stream = snd z1
    r3 <- soapySDRDeviceActivateStream dev stream 0 0 0
    print r3
    let devS = DeviceWithStream dev stream
    return devS

consumeStream :: DeviceWithStream -> IO [CFloat]
consumeStream (DeviceWithStream dev stream) = do
    let num_samples = 1024
    allocaArray (num_samples * 2) $ \buf -> do
        flags <- malloc :: IO (Ptr CInt)
        timeNs <- malloc :: IO (Ptr CLLong)
        with buf $ \bufptr -> do
            elementsRead <- readStream dev stream bufptr (fromIntegral num_samples :: CInt) flags timeNs 1000000
            free flags
            free timeNs
            peekArray (minimum(elementsRead, num_samples * 2)) buf

toComplex :: [CFloat] -> [Complex CDouble]
toComplex floats =
    map (\i -> CDouble (i!!0) :+ CDouble (i!!1)) (chunksOf 2 doubles)
    where doubles = map (float2Double . coerce) floats

toComplexf :: [CFloat] -> [Complex Float]
toComplexf cfloats =
    map (\i -> (i!!0) :+ (i!!1)) (chunksOf 2 floats)
    where floats = map coerce cfloats

fftsetup = do
    inA  <- fftwAllocComplex 1024
    outA <- fftwAllocComplex 1024
    plan <- planDFT1d 1024 inA outA Forward fftwEstimate
    return (inA, outA, plan)

presetfft inA outA plan samples = do
    pokeArray inA samples
    execute plan
    peekArray 1024 outA

fft :: [Complex CDouble] -> IO [Complex CDouble]
fft samples = do
    let n = length samples
    inA  <- fftwAllocComplex (fromIntegral n)
    outA <- fftwAllocComplex (fromIntegral n)
    plan <- planDFT1d n inA outA Forward fftwEstimate

    pokeArray inA samples
    execute plan
    res <- peekArray n outA

    fftwFree inA -- this seems to not work and results in leak
    fftwFree outA -- this seems to not work and results in leak
    return res
