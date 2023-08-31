module SimpleDearImGui.Texture (
    Texture,
    createTexture,
    loadImage,
    drawTexture,
    generateImage,
) where

import Data.Vector.Storable qualified as SV
import DearImGui
import DearImGui.Raw qualified
import Graphics.GL qualified as GL
import RIO

import Data.Massiv.Array (Ix2 ((:.)), Sz (..))
import Data.Massiv.Array qualified as Massiv
import Data.Massiv.Array.IO (Image)
import Data.Massiv.Array.Manifest (S, toStorableVector)
import Foreign qualified
import Graphics.ColorModel qualified as CM

-- | Reference to a texture.
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

-- | Create a black texture.
createTexture :: (Int, Int) -> IO Texture
createTexture imgSize = do
    texture <- create2DTexture imgSize
    loadImage blackImage texture
    pure texture
  where
    blackImage = generateImage imgSize (\_ _ -> CM.PixelRGB 0 0 0)

-- | Create a texture pointer in GL memory.
create2DTexture :: (Int, Int) -> IO Texture
create2DTexture (width, height) =
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

-- | Load an image from CPU to GPU.
loadImage :: Image Massiv.S CM.RGB Word8 -> Texture -> IO ()
loadImage arr texture =
    SV.unsafeWith vec \ptr -> do
        bindTexture texture (Foreign.castPtr ptr)
  where
    vec :: SV.Vector (CM.Pixel CM.RGB Word8)
    vec = toStorableVector arr

{-# ANN drawTexture ("HLint: ignore Avoid lambda" :: String) #-}

-- | Draw the texture.
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

-- | Helper method to create an image per pixel.
generateImage :: (Int, Int) -> (Int -> Int -> CM.Pixel CM.RGB Word8) -> Image S CM.RGB Word8
generateImage (x, y) draw = Massiv.makeArray (Massiv.ParN 0) (Sz (x :. y)) go
  where
    go (i :. j) = draw i j
