module Haskgine2d.Render.Object where


import  Graphics.Rendering.OpenGL (Vector2(..), Color4(..))
import qualified Graphics.Rendering.OpenGL as GL

import Haskgine2d.Render.Shaders (GLProgram(..))


type Position = Vector2 Float
type Color = Color4 Float

{- |
The object that is used for rendering 
-}
data Object = Object {
  glProgram :: GLProgram,

  position  :: Position,
  rotation  :: Float,
  baseColor :: Color,
  
  vbo       :: GL.VertexArrayObject,
  vao       :: GL.BufferObject,
  ibo       :: GL.BufferObject
  }
              
