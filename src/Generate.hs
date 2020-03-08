{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, ScopedTypeVariables, FlexibleInstances, BangPatterns, RecordWildCards #-}

module Generate where

import System.IO
import System.Random
import Control.Monad
import           Control.Monad.Primitive
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Marshal.Utils
import Data.Complex
import qualified Data.List as DL
-- import Data.List.Split
import GHC.Float
import GHC.Word
import Data.Coerce (coerce)
import Data.Array.Repa ((:.)(..), Array, Z(..), DIM2, computeP, fromFunction, (!), toList, slice)
import Data.Array.Repa.Slice
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Array.Repa.IO.Sndfile
import SDR.Filter
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable        as VS
import qualified Data.Vector.Fusion.Bundle         as VFB 
import Pipes
import Data.Vector.Storable.MMap
import Coeffs
import GHC.Prim
import Data.IORef

type Counter = IO Int

num_samples = 128*1024

fsk = do
    array <- allocaArray (num_samples * 2) $ \buf -> do
        let t = [0.0..(fromIntegral num_samples)]
        let f = map (\i -> sin(i/1000)) t
        let i = zipWith (\i j -> CFloat(sin(i/3+j))) t f
        let q = zipWith (\i j -> CFloat(cos(i/3-j))) t f
        let iq = concat (DL.transpose [i, q])
        pokeArray buf iq
        fh <- openBinaryFile "test2.wav" WriteMode
        hPutBuf fh buf (num_samples * 2 * 4)
        putStrLn "wrote file"
    putStrLn "done"

resampled :: Consumer (VS.Vector (Complex Float)) IO ()
resampled = do
    r <- lift $ newIORef 0
    fh <- lift $ openBinaryFile "test-upsampled.wav" WriteMode
    forever $ do
        y <- await
        -- lift $ print (VS.length(y))
        lift $ modifyIORef r (+VS.length(y))
        l <- lift $ readIORef r
        lift $ print l
        lift $ VS.unsafeWith y $ \buf -> do
            hPutBuf fh buf (VS.length(y) * 2 * 4)

test = do
    -- "/home/mike/.fgfs/Aircraft/org.flightgear.fgaddon.stable_2018/Aircraft/an24b/Sound/Engine/AI-24_xShutdown1.wav"
    let fn = "/home/mike/Downloads/sf1_fi1.wav"
    (i, a) <- readSF fn :: IO (Info, Array F DIM2 Double)
    print i
    let w = map double2Float (toList(slice a (Any :. (0::Int) :. All)))
    let cpx = map (\i -> mkPolar 1 (i*5.0)) w :: [Complex Float]
    let ipart = map (CFloat . realPart) cpx
    let qpart = map (CFloat . imagPart) cpx
    let iq = concat (DL.transpose [ipart, qpart])
    let liq = length(iq)
    putStrLn ("lIQ=" ++ show liq)
    rs1 <- fastResamplerCC 16 1 coeffs1
    let res1 = firResampler rs1 16384
    rs2 <- fastResamplerCC 16 1 coeffs2
    let res2 = firResampler rs2 16384
    runEffect $ yield (VS.fromList cpx) >-> res1 >-> res2 >-> resampled
    
    array <- allocaArray (liq) $ \buf -> do
        pokeArray buf iq
        fh <- openBinaryFile "test3.wav" WriteMode
        hPutBuf fh buf (liq * 4)
        putStrLn "wrote file"
    putStrLn "done"
