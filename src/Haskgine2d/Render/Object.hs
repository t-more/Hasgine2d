module Haskgine2d.Render.Object where


import qualified Graphics.Rendering.OpenGL as GL



{-|
An object in the sense of rendering is simple the set of data needed to render it.
-}
data Object = Object {
  glProgram :: GL.Program,
  
  timeUniform :: GL.UniformLocation,
  cameraXUniform :: GL.UniformLocation,
  cameraYUniform :: GL.UniformLocation,
  cameraViewWidthUniform :: GL.UniformLocation,
  cameraViewHeightUniform :: GL.UniformLocation,

  objectPositionXUniform :: GL.UniformLocation,
  objectPositionYUniform :: GL.UniformLocation,
                                              
  vbo :: GL.VertexArrayObject,
  vao :: GL.BufferObject,
  ibo :: GL.BufferObject
                             
  }

   
