{-# LANGUAGE BangPatterns #-}
import Codec.Picture
import qualified Data.ByteString.Lazy as BL
import Criterion.Main
import Control.DeepSeq
import Render

-- main = renderImage 200
main = defaultMain [
    -- bgroup "writeImg" [ bench "100"  $ nfIO ( writeImg 100)
    --         --    , bench "200"  $ whnf writeImg 200
    --         --    , bench "300"  $ whnf writeImg 300
    --         --    , bench "1000" $ whnf writeImg 1000
    --            ], 
    -- bgroup "makeView" [ bench "100"  $ whnf  makeView 100
    --            , bench "200"  $ whnf makeView 200
    --            , bench "300"  $ whnf makeView 300
    --            , bench "1000"  $ whnf makeView 1000
    --            , bench "10000"  $ whnf makeView 10000
    --         --    , bench "1000" $ whnf renderImage 1000
            --    ]
    bgroup "renderImgRecursive" [ bench "100"  $ nfIO  (renderImage 100 True)
               , bench "200"  $ nfIO (renderImage 200 True)
               , bench "300"  $ nfIO (renderImage 300 True)
               , bench "1000" $ nfIO (renderImage 1000 True)
               ],
    bgroup "renderImg" [ bench "100"  $ nfIO  (renderImage 100 False)
               , bench "200"  $ nfIO (renderImage 200 False)
               , bench "300"  $ nfIO (renderImage 300 False)
               , bench "1000" $ nfIO (renderImage 1000 False)
               ]
  ]


writeImg size = do
    writePng "C:\\Users\\jvalk\\Haskell\\RTS\\img.png" $ generateImage pixelRenderer size size
    where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

renderImage size isRecursive =  do
    -- savePngImage "C:\\Users\\jvalk\\Haskell\\RTS\\img1.png" ( renderTest size)
    renderTest size isRecursive

makeView size = do
    let a = (makeViewTest size) !! (fromInteger size -1) !! (fromInteger size - 1)
    return 1
