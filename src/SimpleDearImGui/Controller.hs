module SimpleDearImGui.Controller (
    -- * Create 'Controller'
    Controller,
    newSliderInt,
    newSliderFloat,

    -- * Render 'Controller'
    renderControllers,

    -- * Read value from 'Controller'
    readControllerInt,
    readControllerFloat,
    readControllerDouble,
) where

import DearImGui qualified
import GHC.Float (float2Double)
import RIO

data Controller
    = SliderInt Text Int Int (IORef Int)
    | SliderFloat Text Float Float (IORef Float)

----------------------------------------
-- Controller helpers
----------------------------------------
newSliderInt :: Text -> Int -> IO Controller
newSliderInt name v = SliderInt name 0 12 <$> newIORef v

newSliderFloat :: Text -> Float -> Float -> IO Controller
newSliderFloat name v maxValue = SliderFloat name 0 maxValue <$> newIORef v

readControllerInt :: Controller -> IO Int
readControllerInt = \case
    SliderInt _ _ _ ref -> readIORef ref
    SliderFloat _ _ _ ref -> round <$> readIORef ref

readControllerFloat :: Controller -> IO Float
readControllerFloat = \case
    SliderInt _ _ _ ref -> fromIntegral <$> readIORef ref
    SliderFloat _ _ _ ref -> readIORef ref

readControllerDouble :: Controller -> IO Double
readControllerDouble = fmap float2Double . readControllerFloat

renderController :: Controller -> IO Bool
renderController = \case
    SliderInt name minValue maxValue ref -> DearImGui.sliderInt name ref minValue maxValue
    SliderFloat name minValue maxValue ref -> DearImGui.sliderFloat name ref minValue maxValue

renderControllers :: IO () -> [Controller] -> IO ()
renderControllers action = DearImGui.withGroup . traverse_ doRender
  where
    doRender controller = DearImGui.withItemWidth 200 do
        whenM (renderController controller) action
