module Haskgine2d.Render.Shaders (ShaderData(..), ProgramPool(..), ProgramShaders(..), GLProgram(..), loadShaders, makeProgramPool) where
{- |
Author: Tomas Möre
TODO this is currently really unsafe. Make it better 
-}
import qualified System.FilePath as FP
import qualified Graphics.Rendering.OpenGL as GL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad

import qualified Data.ByteString as BS
import Data.StateVar (($=!), get) 
 
import Data.Monoid
import Data.Maybe

data ShaderData a = ShaderData {
  vertex   :: a,
  fragment :: a
  }
instance Functor ShaderData where
  fmap f (ShaderData a b) = ShaderData (f a) (f b)
  
instance Monoid m => Monoid (ShaderData m) where
  mempty = ShaderData mempty mempty
  mappend (ShaderData a1 b1) (ShaderData a2 b2) = ShaderData (a1 <> a2) (b1 <> b2)
  
type ShaderPool = ShaderData (Map String GL.Shader)


    
type ProgramPool = Map String GLProgram
 
type ProgramTemplatePool = Map String ProgramTemplate
type ProgramTemplate = ShaderData String
type ProgramShaders = ShaderData GL.Shader

{- |
The object containing the bindings necesary to render an object in haskgine.
This means that every shader program is required to have these fields to work.
The unforms are currently named the same things as the fields in this record.
-}
data GLProgram = GLProgram {
  glProgram               :: GL.Program,
  
  time                    :: GL.UniformLocation, -- Float
  cameraPosition          :: GL.UniformLocation, -- Vec2 Float
  cameraViewMatrics       :: GL.UniformLocation, -- Vec2 Float
  objectPositon           :: GL.UniformLocation, -- Vec2 Float
  objectRotation          :: GL.UniformLocation, -- Float
  objectBaseColor         :: GL.UniformLocation -- Color4 Float
  }
emptyProgram = GLProgram                 
 
{- |
Takes a list of filepaths to shaders and converts them into a shaderpool if possible
Expects the filenanmes of each shader to be unique.
-}
loadShaders :: [FilePath] -> IO ShaderPool
loadShaders [] = return mempty 
loadShaders (shaderFP:rest)  = do
  shader <- GL.createShader shaderType
  shaderBS <- BS.readFile shaderFP
  GL.shaderSourceBS shader $=! shaderBS 
  GL.compileShader shader
  compileOK <- GL.compileStatus shader
  unless compileOK $ do
    errorStr <- GL.shaderInfoLog shader
    error ("Couldn't compile shader " <> shaderFP <>  " shader: \n Errors: \n" <> errorStr)
  mappend (singletonPool shader) <$> loadShaders rest
  where
    name = FP.takeBaseName shaderFP 
    shaderType = case FP.takeExtension shaderFP of
      "frag" -> GL.FragmentShader
      "vert" -> GL.VertexShader
      _ -> error $ "File must end with a valid file ending: " <> shaderFP
    singletonPool shader = case shaderType of
      GL.FragmentShader -> mempty{ fragment = Map.singleton name shader}
      GL.VertexShader   -> mempty{ vertex = Map.singleton name shader}
      _ -> error "Shader format not supported."

{- |
 Takes a map of programs and their templates.
 Will filter out programs that are the same and make them aliases.
-}

makeProgramPool :: [(String, ProgramTemplate)] -> ShaderPool -> IO ProgramPool
makeProgramPool [] _ = pure mempty
makeProgramPool programs shaderPool = mconcat <$> mapM (\ (name, template) -> Map.singleton name <$> compileProgram (fromJust $ lookupShaders template shaderPool)) programs



lookupShaders :: ProgramTemplate -> ShaderPool -> Maybe ProgramShaders
lookupShaders name pool = ShaderData <$> Map.lookup (vertex name) (vertex pool)
                                     <*> Map.lookup (fragment name) (fragment pool)

compileProgram :: ProgramShaders -> IO GLProgram
compileProgram shader = do
  program <- GL.createProgram
  GL.attachShader program $ vertex shader
  GL.attachShader program $ fragment shader
  GL.linkProgram program
  bindProgramData program

bindProgramData :: GL.Program -> IO GLProgram
bindProgramData program = GLProgram <$> pure program
                                    <*> getUniform "time"
                                    <*> getUniform "cameraPosition"
                                    <*> getUniform "cameraViewMatrics"
                                    <*> getUniform "objectPosition"
                                    <*> getUniform "objectRotation"
                                    <*> getUniform "objectBaseColor"
                                   
  where
    getUniform = get . GL.uniformLocation program

