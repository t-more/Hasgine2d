{-# LANGUAGE QuasiQuotes #-}
module Haskgine2d.Render.DefaultShaders where

import Data.String
import qualified  Data.ByteString as B 
import Haskgine2d.Render.Shaders

import qualified Data.Map as Map

defaultShaderSources :: ShaderSourcePool
defaultShaderSources = ShaderData (Map.singleton "Default" defaultVertex) (Map.singleton "Default" defaultFragment)

defaultVertex :: B.ByteString 
defaultVertex = fromString " #version 330 \
                           \ layout (location = 0) in vec3 vertex; \                           
                           \ uniform float time; \
                           \ uniform vec3  cameraPosition; \
                           \ uniform float cameraRotation; \
                           \ uniform vec2 cameraViewMatrics; \
                           
                           \ uniform vec2  objectPosition; \
                           \ uniform float objectRotation; \
                           \ uniform vec4  objectBaseColor; \
                           \ uniform vec2 objectScale; \
                           
                           \ out vec3 fpos; \
                           
                           \ mat2 rotmat(float n){ \
                           \     return mat2( cos(n), -sin(n), sin(n),cos(n)); \
                           \ } \                           
                           \ void main(){ \
                           \    fpos = vertex; \                           
                           \    vec2 pos_2d = vertex.xy; \
                           \    pos_2d.x *= objectScale.x; \
                           \    pos_2d.y *= objectScale.y; \                           
                           \    pos_2d *= rotmat(objectRotation); \
                           \    pos_2d.x += objectPosition.x; \
                           \    pos_2d.y += objectPosition.y; \                           
                           \    pos_2d.x *= cameraViewMatrics.y / cameraViewMatrics.x ; \
                           \    gl_Position = vec4(pos_2d.x,pos_2d.y, vertex.z, 1.0); \
                           \ }"

defaultFragment :: B.ByteString 
defaultFragment = fromString " #version 330 \                             
                             \ uniform float time; \
                             \ uniform vec3  cameraPosition; \
                             \ uniform float cameraRotation; \
                             \ uniform vec2 cameraViewMatrics; \
                             
                             \ uniform vec2  objectPosition; \
                             \ uniform float objectRotation; \
                             \ uniform vec4  objectBaseColor; \
                                                          
                             \ in vec3 fragment_pos; \                             
                             \ out vec4 color; \
                             
                             
                             \ void main(){ \
                             \     color = objectBaseColor; \
                             \ }"
