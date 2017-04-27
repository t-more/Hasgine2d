module Main where

import Haskgine2d 
import Haskgine2d.Render.Shaders
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
  window <- fromMaybe <$> initGLFW (1920,1080)
  
  shaderPool <- loadShaders $  ["shaders/HelloTriangle.frag", "shaders/HelloTriangle.vert"]
  programPool <- makeProgramPool [("HelloTriangle", ShaderData "HelloTriangle" "HelloTriangle")] shaderPool
  let program = Map.lookup programPool "HelloTriangel"
  print program
  putStrLn "Done"
 
 

{- Initates GLFW -}
initGLFW :: (Int,Int) -> IO (Maybe GLFW.Window)
initGLFW (windowWidth, windowHeight) = do
  initSuccess <- GLFW.init
  unless initSuccess (putStrLn "Couldn't init glfw")
  GLFW.windowHint $ GLFW.WindowHint'Samples 1
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'RefreshRate 60
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'Resizable True
  window <- GLFW.createWindow windowWidth windowHeight "Hello triangle" Nothing Nothing
  when (isNothing window) (putStrLn "Couldn't create window")
  
  GLFW.makeContextCurrent window

  GL.depthFunc $= Just GL.Lequal
  return window
