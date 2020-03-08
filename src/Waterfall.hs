{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Monad.Trans.Except
import Control.Error.Util
import Control.Concurrent
import Pipes
import qualified Pipes.Prelude as P
import System.Random
import Data.Complex
import Foreign.C.Types
import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Window
import Graphics.Rendering.OpenGL
import SoapySDRUtils
import System.Mem (performGC)
import Options.Applicative

data Options = Options {
    frequency  :: Double,
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
    <*> option auto (
        long "samplerate"   
        <> short 's' 
        <> metavar "SAMPLERATE"  
        <> help "Sample rate, Hz"
        )

opt :: ParserInfo Options
opt = info (helper <*> optParser) (fullDesc <> progDesc "Display RF spectrum waterfall" <> header "RTLSDR waterfall")


fftpipe :: Double -> Double -> Producer [GLfloat] IO ()
fftpipe freq bw = do
    dev <- lift $ setupStream "driver=rtlsdr" freq bw
    (inA, outA, plan) <- lift fftsetup
    forever $ do
        samples <- lift (consumeStream dev)
        let compl = toComplex samples
        ffts <- lift (presetfft inA outA plan compl)
        let reals = map (realToFrac . magnitude) ffts
        -- https://www.gaussianwaves.com/2015/11/interpreting-fft-results-complex-dft-frequency-bins-and-fftshift/
        let rf = take 512 (drop 1 reals)
        let lf = drop 512 reals
        lift performGC
        Pipes.yield (lf ++ rf)

doIt Options{..} = exceptT putStrLn return $ do
    res <- lift setupGLFW
    unless res (throwE "Unable to initilize GLFW")
    waterfall <- window 1024 480 $ renderWaterfall 1024 1000 jet_mod
    lift $ runEffect $ (fftpipe frequency samplerate) >-> waterfall

main = do
    op <- execParser opt
    doIt op