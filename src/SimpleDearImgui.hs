module SimpleDearImgui where

import Control.Monad.Managed (managed, managed_, runManaged)
import Data.Vector.Storable qualified as SV
import DearImGui
import DearImGui.OpenGL3 qualified
import DearImGui.Raw qualified
import DearImGui.SDL qualified
import DearImGui.SDL.OpenGL qualified
import GHC.Float (float2Double)
import Graphics.GL qualified as GL
import RIO
import SDL hiding (Texture, textureHeight, textureWidth)

import Data.Massiv.Array (Ix2 ((:.)), Sz (..))
import Data.Massiv.Array qualified as Massiv
import Data.Massiv.Array.IO (Image)
import Data.Massiv.Array.Manifest (S, toStorableVector)
import Foreign qualified
import Graphics.ColorModel qualified as CM

import Numeric.Noise.Perlin qualified as Perlin

-- | Helper method to create an image per pixel
renderPixel :: V2 Int -> (Int -> Int -> CM.Pixel CM.RGB Word8) -> Image S CM.RGB Word8
renderPixel (V2 x y) draw = Massiv.makeArray (Massiv.ParN 0) (Sz (x :. y)) go
  where
    go (i :. j) = draw i j

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
            texture <- loadImage (renderPixel imgSize (\_ _ -> CM.PixelRGB 0 0 0))
            renderTexture controllers texture
            pure (controllers, texture)
        , sceneHandle = \(_controllers, _textures) _ev -> pure ()
        , sceneRender = \(controllers, texture) -> do
            withFullscreen do
                text $ "progcon - " <> controllers.name
                DearImGui.plotLines "samples" [sin (x / 10) | x <- [0 .. 64]]
                drawTexture texture

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
        scale <- float2Double <$> readControllerFloat controllers.scale
        depth <- float2Double <$> readControllerFloat controllers.depth
        putStrLn $ "Regen noise with " <> show seed
        let perlinNoise = Perlin.perlin seed octaves scale persistance
            draw x y =
                let i = fromIntegral x
                    j = fromIntegral y
                    v = Perlin.noiseValue perlinNoise (i, j, depth)
                    uv = round (v * 255)
                 in CM.PixelRGB uv uv uv

        loadData (renderPixel imgSize draw) texture
    persistance = 0.5

    imgSize = V2 500 500

data Scene a = Scene
    { sceneInit :: IO a
    -- ^ Setup globals and textures
    , sceneHandle :: a -> SDL.Event -> IO ()
    -- ^ Handle keyboard and mouse events
    , sceneRender :: a -> IO ()
    -- ^ Render dear-imgui interface
    }

runDemo :: IO ()
runDemo = runScene noiseGui

----------------------------------------
-- Controller helpers
----------------------------------------
newSliderInt :: Text -> Int -> IO Controller
newSliderInt name v = SliderInt name (0) 12 <$> newIORef v

newSliderFloat :: Text -> Float -> Float -> IO Controller
newSliderFloat name v maxValue = SliderFloat name (0) maxValue <$> newIORef v

readControllerInt :: Controller -> IO Int
readControllerInt = \case
    SliderInt _ _ _ ref -> readIORef ref
    SliderFloat _ _ _ ref -> round <$> readIORef ref

readControllerFloat :: Controller -> IO Float
readControllerFloat = \case
    SliderInt _ _ _ ref -> fromIntegral <$> readIORef ref
    SliderFloat _ _ _ ref -> readIORef ref

renderController :: Controller -> IO Bool
renderController = \case
    SliderInt name minValue maxValue ref -> DearImGui.vSliderInt name sz ref minValue maxValue
    SliderFloat name minValue maxValue ref -> DearImGui.vSliderFloat name sz ref minValue maxValue
  where
    sz = DearImGui.ImVec2 10 500

data Controller
    = SliderInt Text Int Int (IORef Int)
    | SliderFloat Text Float Float (IORef Float)

renderControllers :: IO () -> [Controller] -> IO ()
renderControllers action = traverse_ doRender
  where
    doRender controller = do
        DearImGui.sameLine
        whenM (renderController controller) action

----------------------------------------
-- Texture helpers
----------------------------------------
data Texture = Texture
    { textureID :: GL.GLuint
    , textureWidth :: GL.GLsizei
    , textureHeight :: GL.GLsizei
    }
    deriving (Show)

textureSize :: Texture -> DearImGui.ImVec2
textureSize texture =
    DearImGui.ImVec2
        (fromIntegral $ texture.textureWidth)
        (fromIntegral $ texture.textureHeight)

-- | Create a texture pointer in GL memory.
create2DTexture :: Int -> Int -> IO Texture
create2DTexture width height =
    Foreign.alloca \ptr -> do
        GL.glGenTextures 1 ptr
        tID <- Foreign.peek ptr
        pure
            Texture
                { textureID = tID
                , textureWidth = fromIntegral width
                , textureHeight = fromIntegral height
                }

bindTexture :: Texture -> Foreign.Ptr GL.GLubyte -> IO ()
bindTexture texture dataPtr = do
    GL.glEnable GL.GL_TEXTURE_2D
    GL.glBindTexture GL.GL_TEXTURE_2D texture.textureID

    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER GL.GL_LINEAR
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER GL.GL_LINEAR
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S GL.GL_REPEAT
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T GL.GL_REPEAT

    GL.glTexImage2D
        GL.GL_TEXTURE_2D
        0
        GL.GL_RGB
        texture.textureWidth
        texture.textureHeight
        0
        GL.GL_RGB
        GL.GL_UNSIGNED_BYTE
        (Foreign.castPtr dataPtr)

loadImage :: Image Massiv.S CM.RGB Word8 -> IO Texture
loadImage arr = do
    texture <- create2DTexture x y
    loadData arr texture
    pure texture
  where
    Sz (x :. y) = Massiv.size arr

loadData :: Image Massiv.S CM.RGB Word8 -> Texture -> IO ()
loadData arr texture =
    SV.unsafeWith vec \ptr -> do
        bindTexture texture (Foreign.castPtr ptr)
  where
    vec :: SV.Vector (CM.Pixel CM.RGB Word8)
    vec = toStorableVector arr

{-# ANN drawTexture ("HLint: ignore Avoid lambda" :: String) #-}
drawTexture :: Texture -> IO ()
drawTexture texture =
    Foreign.with (textureSize texture) \sizePtr ->
        Foreign.with (DearImGui.ImVec2 0 0) \uv0Ptr ->
            Foreign.with (DearImGui.ImVec2 1 1) \uv1Ptr ->
                Foreign.with (DearImGui.ImVec4 1 1 1 1) \tintColPtr ->
                    Foreign.with (DearImGui.ImVec4 1 1 1 1) \borderColPtr ->
                        DearImGui.Raw.image openGLtextureID sizePtr uv0Ptr uv1Ptr tintColPtr borderColPtr
  where
    openGLtextureID = Foreign.intPtrToPtr $ fromIntegral texture.textureID

----------------------------------------
-- Engine
----------------------------------------
runScene :: Scene a -> IO ()
runScene scene = do
    initializeAll

    let keyHandler resources event
            | isQuit = pure True
            | otherwise = do
                scene.sceneHandle resources event
                pure False
          where
            keyCode = case eventPayload event of
                KeyboardEvent ke | ke.keyboardEventKeyMotion == Pressed -> Just ke.keyboardEventKeysym.keysymScancode
                _ -> Nothing
            isQuit =
                SDL.eventPayload event == SDL.QuitEvent
                    || keyCode == Just ScancodeEscape

    runManaged do
        window <- do
            let title = "SimpleDearImgui"
            let config = defaultWindow{windowGraphicsContext = OpenGLContext defaultOpenGL}
            managed $ bracket (createWindow title config) destroyWindow
        glContext <- managed $ bracket (glCreateContext window) glDeleteContext
        _ <- managed $ bracket createContext destroyContext
        _ <- managed_ $ bracket_ (DearImGui.SDL.OpenGL.sdl2InitForOpenGL window glContext) DearImGui.SDL.sdl2Shutdown
        _ <- managed_ $ bracket_ DearImGui.OpenGL3.openGL3Init DearImGui.OpenGL3.openGL3Shutdown
        resources <- liftIO scene.sceneInit
        liftIO $ mainLoop window (keyHandler resources) (scene.sceneRender resources)

mainLoop :: Window -> (Event -> IO Bool) -> IO () -> IO ()
mainLoop window eventHandler renderUI = fix \loop -> do
    DearImGui.OpenGL3.openGL3NewFrame
    DearImGui.SDL.sdl2NewFrame
    DearImGui.newFrame
    renderUI
    GL.glClear GL.GL_COLOR_BUFFER_BIT
    DearImGui.render
    DearImGui.OpenGL3.openGL3RenderDrawData =<< getDrawData
    SDL.glSwapWindow window

    shouldQuits <- traverse eventHandler =<< DearImGui.SDL.pollEventsWithImGui
    unless (or shouldQuits) loop
