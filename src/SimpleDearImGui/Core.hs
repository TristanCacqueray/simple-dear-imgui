-- | This module contains the core of 'simple-dear-imgui'.
module SimpleDearImGui.Core (Scene (..), runScene) where

import Control.Monad.Managed (managed, managed_, runManaged)
import DearImGui qualified
import DearImGui.OpenGL3 qualified
import DearImGui.SDL qualified
import DearImGui.SDL.OpenGL qualified
import Graphics.GL qualified as GL
import RIO
import SDL hiding (Texture, textureHeight, textureWidth)

data Scene state = Scene
    { sceneInit :: IO state
    -- ^ Setup globals and textures
    , sceneHandle :: state -> SDL.Event -> IO ()
    -- ^ Handle keyboard and mouse events
    , sceneRender :: state -> IO ()
    -- ^ Render user interface
    }

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
            let title = "SimpleDearImGui"
            let config = defaultWindow{windowGraphicsContext = OpenGLContext defaultOpenGL}
            managed $ bracket (createWindow title config) destroyWindow
        glContext <- managed $ bracket (glCreateContext window) glDeleteContext
        _ <- managed $ bracket DearImGui.createContext DearImGui.destroyContext
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
    DearImGui.OpenGL3.openGL3RenderDrawData =<< DearImGui.getDrawData
    SDL.glSwapWindow window

    shouldQuits <- traverse eventHandler =<< DearImGui.SDL.pollEventsWithImGui
    unless (or shouldQuits) loop
