module Main (main) where

import DearImGui
import Graphics.Pixel (Pixel (PixelRGB))
import Numeric.Noise.Perlin qualified as Perlin
import RIO

import SimpleDearImGui.Controller
import SimpleDearImGui.Core
import SimpleDearImGui.Texture

-- | A demo UI to edit a noise texture
data NoiseControllers = NoiseControllers
    { name :: Text
    , seed :: Controller
    , octaves :: Controller
    , scale :: Controller
    , depth :: Controller
    }

-- | The UI definition
noiseGui :: Scene (NoiseControllers, Texture)
noiseGui =
    Scene
        { sceneInit = do
            controllers <-
                NoiseControllers "perlin"
                    <$> newSliderInt "seed" 1
                    <*> newSliderInt "octaves" 2
                    <*> newSliderFloat "scales" 0.05 0.5
                    <*> newSliderFloat "depth" 0 42
            texture <- createTexture imgSize
            renderTexture controllers texture
            pure (controllers, texture)
        , sceneHandle = \(_controllers, _textures) _ev -> pure ()
        , sceneRender = \(controllers, texture) -> do
            withFullscreen do
                text $ controllers.name
                DearImGui.plotLines "samples" [sin (x / 10) | x <- [0 .. 64]]
                drawTexture texture
                DearImGui.sameLine

                let onUpdate = renderTexture controllers texture
                renderControllers
                    onUpdate
                    [controllers.seed, controllers.octaves, controllers.scale, controllers.depth]
        }
  where
    renderTexture :: NoiseControllers -> Texture -> IO ()
    renderTexture controllers texture = do
        seed <- readControllerInt controllers.seed
        octaves <- readControllerInt controllers.octaves
        scale <- readControllerDouble controllers.scale
        depth <- readControllerDouble controllers.depth
        putStrLn $ "Regen noise with " <> show seed
        let perlinNoise = Perlin.perlin seed octaves scale persistance
            draw x y =
                let i = fromIntegral x
                    j = fromIntegral y
                    v = Perlin.noiseValue perlinNoise (i, j, depth)
                    uv = round (v * 255)
                 in PixelRGB uv uv uv

        loadImage (generateImage imgSize draw) texture
    persistance = 0.5

    imgSize = (500, 500)

main :: IO ()
main = runScene noiseGui
