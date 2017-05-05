module Main where

import Haskgine2d 
import Haskgine2d.Render.Shaders
import Haskgine2d.Render.Shapes as Shapes
import Haskgine2d.Render.Object
import Haskgine2d.Render.Context 

import Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

import Data.StateVar

main :: IO ()
main = do
  window <- fromJust <$> initGLFW (1920,1080)
  
  shaderPool <- loadShaders $  ["shaders/HelloTriangle.frag", "shaders/HelloTriangle.vert"]
  protoContext <- createContext <$> makeProgramPool [("HelloTriangle", ShaderData "HelloTriangle" "HelloTriangle")] shaderPool
  let program = fromJust $ findProgram protoContext "HelloTriangle"
      sphereShape = equilategralPolygon 50
  sphereObjectTemplate <- toObject program (vertices sphereShape) (indices sphereShape)
  let templateContext = addObjectTemplate protoContext "Circle" sphereObjectTemplate
      sphereTemplate = fromJust $ getObjectTemplate templateContext "Circle"
      (objectID, drawContext) = addObject templateContext sphereTemplate{ baseColor = GL.Color4 0 1 0 1}
  let loop context = do
        GLFW.pollEvents
        
        GL.clearColor $= GL.Color4 0 0 0 0
        GL.clear [ GL.ColorBuffer ]

        drawAll context
        
        GLFW.swapBuffers window
        let tick = flip increaseTime  0.01
        loop $ (tick $ alterObject context objectID (\ obj -> obj{ position = GL.Vector2 (sin (time context)) 0} )) 
      
  loop drawContext
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
  GL.frontFace $=! GL.CW
  --GL.depthFunc $=! Just GL.Gequal
  return window
