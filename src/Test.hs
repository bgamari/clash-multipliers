{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test where

import Clash.Prelude
import Data.Functor.Compose
import Data.Proxy
import Debug.Trace

{-# ANN test
    ((defSyn "test")
             { t_name = "test"
             , t_inputs = [ PortName "clock"
                          , PortName "reset"
                          , PortName "x"
                          , PortName "y"
                          ]
             , t_output = PortName "out"
             })
 #-}

test :: Clock System Source
     -> Reset System Synchronous
     -> Signal System (Unsigned N)
     -> Signal System (Unsigned N)
     -> Signal System (Unsigned N)
test clk reset x y = withClockReset clk reset $ fmap fromIntegral $ mul4  x y

-- | Latency of 2 cycles
mul4 :: HiddenClockReset dom gated synth
     => Signal dom (Unsigned 16)
     -> Signal dom (Unsigned 16)
     -> Signal dom (Unsigned 16)
mul4 x y =
    register 0 $ register 0 (a + b) + register 0 (c + d)
  where
    chunk :: Unsigned 16 -> Unsigned 4
    chunk = fromIntegral

    mulChunk :: Int -> Unsigned 16 -> Unsigned 16 -> Unsigned 16
    mulChunk n x y =
        (`shiftL` n)
        $ fromIntegral
        $ chunk (x `shiftR` n) `times` chunk (y `shiftR` n)

    a = mulChunk 0 <$> x <*> y
    b = mulChunk 4 <$> x <*> y
    c = mulChunk 8 <$> x <*> y
    d = mulChunk 12 <$> x <*> y

--tr = fmap (\x -> traceShow x x)

mul :: forall chunk n nChunks dom gated synth.
       (KnownNat chunk, KnownNat n, KnownNat nChunks,
        HiddenClockReset dom gated synth,
        (2^n) ~ (chunk*nChunks), 1 <= nChunks)
    => Signal dom (Unsigned (2^n))
    -> Signal dom (Unsigned (2^n))
    -> Signal dom (Unsigned (2*(n+1)))
mul x y =
    fmap sum $ getCompose $ fmap fromIntegral partials'
  where
    chunks :: Unsigned (2^n) -> Vec nChunks (Unsigned chunk)
    chunks n = 
        fmap (\i -> fromIntegral $ n `shiftR` (chunkSz * fromIntegral i)) indicesI

    chunkSz = fromIntegral $ natVal $ Proxy @chunk

    partials' :: Compose (Signal dom) (Vec nChunks) (Unsigned (2*(n+1)))
    partials' = shiftL <$> fmap fromIntegral partials <*> Compose (pure $ fmap (*chunkSz) $ fmap fromIntegral indicesI)

    partials :: Compose (Signal dom) (Vec nChunks) (Unsigned chunk)
    partials =
        (*) <$> Compose (fmap chunks x) <*> Compose (fmap chunks y)

mul3 :: HiddenClockReset dom gated synth
     => Signal dom (Unsigned 16)
     -> Signal dom (Unsigned 16)
     -> Signal dom (Unsigned 16)
mul3 x y =
    register 0 $ register 0 (a0 + a1) + register 0 (a2 + a3) + register 0 (a4 + a5)
  where
    chunk :: Unsigned 16 -> Unsigned 3
    chunk = fromIntegral

    mulChunk :: Int -> Unsigned 16 -> Unsigned 16 -> Unsigned 16
    mulChunk n x y =
        (`shiftL` n)
        $ fromIntegral
        $ chunk (x `shiftR` n) `times` chunk (y `shiftR` n)

    a0 = mulChunk 0 <$> x <*> y
    a1 = mulChunk 3 <$> x <*> y
    a2 = mulChunk 6 <$> x <*> y
    a3 = mulChunk 9 <$> x <*> y
    a4 = mulChunk 12 <$> x <*> y
    a5 = mulChunk 15 <$> x <*> y
    a6 = mulChunk 18 <$> x <*> y

naive :: HiddenClockReset dom gated synth
      => Signal dom (Unsigned 16)
      -> Signal dom (Unsigned 16)
      -> Signal dom (Unsigned 16)
naive x y = register 0 $ register 0 $ x*y

--dadda :: HiddenClockReset dom gated synth
--      => Signal dom (Unsigned 16)
--      -> Signal dom (Unsigned 16)
--      -> Signal dom (Unsigned 16)
--dadda x y = undefined

type N = 16
