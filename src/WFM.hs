import Control.Monad
import Control.Monad.Trans.Except
import Control.Concurrent
import Control.Error.Util
import Pipes
import qualified Pipes.Prelude as P
import Data.Complex
import Foreign.C.Types
import SoapySDRUtils
import Data.Coerce (coerce)
import System.Mem (performGC)

import SDR.Filter 
import SDR.Util
import SDR.Demod
import SDR.Pulse
import SDR.CPUID
import SDR.PipeUtils(combine)

import Coeffs
import qualified Data.Vector.Storable as VS
import Graphics.Rendering.OpenGL
import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Window
import System.Random
import GHC.Float
import GHC.Word

rcvstream :: Pipe [CFloat] (VS.Vector (Complex Float)) IO ()
rcvstream = forever $ do
    samples <- await
    let compl = toComplexf samples
    Pipes.yield (VS.fromList compl)

stream :: Producer [CFloat] IO ()
stream = do
    dev <- lift $ setupStream "driver=rtlsdr" 105.6e6 1.28e6
    forever $ do
        samples <- lift (consumeStream dev)
        Pipes.yield samples

fftpipe :: Pipe [CFloat] [GLfloat] IO ()
fftpipe = forever $ do
    samples <- await
    (inA, outA, plan) <- lift fftsetup
    let compl = toComplex samples
    ffts <- lift (presetfft inA outA plan compl)
    let reals = map (realToFrac . magnitude) ffts
    lift performGC
    Pipes.yield reals

fftpipe2 :: Pipe (VS.Vector (Complex Float)) [GLfloat] IO ()
fftpipe2 = forever $ do
    samples <- await
    (inA, outA, plan) <- lift fftsetup
    let complFloatList = VS.toList samples
    let compl = map (\i -> CDouble (float2Double (realPart i)) :+ CDouble (float2Double(imagPart i))) complFloatList
    ffts <- lift (presetfft inA outA plan compl)
    let reals = map (realToFrac . magnitude) ffts
    lift performGC
    Pipes.yield reals

nsamples = 1024

main = exceptT putStrLn return $ do
    res <- lift setupGLFW
    info <- lift getCPUInfo
    sink <- lift pulseAudioSink

    deci <- lift $ fastDecimatorC info 40 coeffsRFDecim -- 1280kHz->32kHz
    rffilt <- lift $ fastFilterC info coeffsRFDecim2
    resp <- lift $ fastResamplerR info 3 2 coeffsAudioResampler

    filt <- lift $ fastFilterSymR info coeffsAudioFilter2
    waterfall <- window 1024 480 $ renderWaterfall 1000 1000 jet_mod

    lift $ runEffect $ stream
        >-> foldl1 combine [
            rcvstream
                >-> firDecimator deci nsamples
                >-> fmDemod
                >-> firResampler resp nsamples
                >-> firFilter filt nsamples
                >-> sink,
            rcvstream >-> firFilter rffilt nsamples >-> fftpipe2 >-> waterfall
        ]