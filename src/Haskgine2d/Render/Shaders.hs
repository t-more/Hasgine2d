module Haskgine2d.Render.Shaders ( ProgramTemplate
                                 , ShaderData(..)
                                 , ShaderErrors
                                 , ShaderSourcePool
                                 , ProgramPool
                                 , ProgramShaders
                                 , GLProgram
                                 , setTime
                                 , setCameraPosition
                                 , setCameraRotation
                                 , setCameraViewMatrics
                                 , setObjectPositon
                                 , setObjectRotation
                                 , setObjectBaseColor
                                 , setObjectScale
                                 , glProgram
                                 , loadShaders
                                 , makeProgramPool
                                 , compileShaderSources
                                 , loadShaderSources
                                 ) where
{- |
Author: Tomas Möre
TODO this is currently really unsafe. Make it better 
-}
import qualified System.FilePath as FP
import qualified Graphics.Rendering.OpenGL as GL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad

import qualified Data.ByteString as B
import Data.StateVar (($=!), get) 
 
import Data.Monoid
import Data.Either 

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

type ShaderSourcePool = ShaderData (Map String B.ByteString)
type ShaderErrors = ShaderData [String]
     
type ProgramPool = Map String GLProgram  
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
  cameraRotation          :: GL.UniformLocation, -- Float
  cameraViewMatrics       :: GL.UniformLocation, -- Vec2 Float
  objectPosition          :: GL.UniformLocation, -- Vec2 Float
  objectRotation          :: GL.UniformLocation, -- Float
  objectBaseColor         :: GL.UniformLocation, -- Color4 Float
  objectScale             :: GL.UniformLocation
  } deriving (Show)

instance Eq GLProgram where
  (==) a b = glProgram a == glProgram b

setUniform :: (GL.Uniform v) => (GLProgram -> GL.UniformLocation) -> GLProgram -> v -> IO ()
setUniform attr object value = GL.uniform (attr object) $=! value

setTime :: GLProgram -> Float -> IO ()
setTime = setUniform time

setCameraPosition :: GLProgram -> GL.Vector2 Float -> IO ()
setCameraPosition = setUniform cameraPosition

setCameraRotation :: GLProgram -> Float -> IO ()
setCameraRotation = setUniform cameraRotation

setCameraViewMatrics :: GLProgram -> GL.Vector2 Float -> IO ()
setCameraViewMatrics = setUniform cameraViewMatrics

setObjectPositon :: GLProgram -> GL.Vector2 Float -> IO ()
setObjectPositon = setUniform objectPosition

setObjectRotation :: GLProgram -> Float -> IO ()
setObjectRotation = setUniform objectRotation

setObjectBaseColor :: GLProgram -> GL.Color4 Float -> IO ()
setObjectBaseColor = setUniform objectBaseColor

setObjectScale :: GLProgram -> GL.Vector2 Float -> IO ()
setObjectScale = setUniform objectScale

{- | Loads a list of filepaths to shaders and organises the source code
into the diffrent shader types
-}
loadShaderSources :: [FilePath] -> IO ShaderSourcePool
loadShaderSources [] = mempty
loadShaderSources (fp:fps) = do
  source  <- B.readFile fp
  rest <- loadShaderSources fps
  return $ rest <> (case shaderType of
    GL.FragmentShader -> ShaderData{ vertex = mempty, fragment = Map.singleton name source}
    GL.VertexShader -> ShaderData{ vertex = Map.singleton name source, fragment = mempty})
    where
    name = FP.takeBaseName fp 
    shaderType = case FP.takeExtension fp of
      ".frag" -> GL.FragmentShader
      ".vert" -> GL.VertexShader
      _ -> error $ "File must end with a valid file ending: " <> fp

{-|
Compiles the shadersources
-}
compileShaderSources :: ShaderSourcePool -> IO (Either ShaderErrors ShaderPool)
compileShaderSources sourcePool = do
  vertexResults <- mapM (compileShaderSource GL.VertexShader) $ vertex sourcePool
  fragmentResults <- mapM (compileShaderSource GL.FragmentShader) $ fragment sourcePool
  let vertexErrors = lefts $ Map.elems vertexResults
      fragmentErrors = lefts $ Map.elems fragmentResults
  if not (null vertexErrors || null fragmentErrors)
    then return $ Left $ ShaderData vertexErrors fragmentErrors 
    else return $ Right $ ShaderData{
    vertex = fromRight <$> vertexResults,
    fragment = fromRight <$> fragmentResults
    }
  where
    fromRight (Right e) = e
      
{- |
Compiles a specific shader and checks for errors 
-}
compileShaderSource :: GL.ShaderType -> B.ByteString -> IO (Either String GL.Shader)
compileShaderSource shaderType source = do
  shader <- GL.createShader shaderType
  GL.shaderSourceBS shader $=! source
  GL.compileShader shader
  compileOK <- GL.compileStatus shader
  if compileOK
    then return $ Right shader
    else Left <$> GL.shaderInfoLog shader

{- |
Takes a list of filepaths to shaders and converts them into a shaderpool if possible
Expects the filenanmes of each shader to be unique.
-}

loadShaders :: [FilePath] -> IO (Either ShaderErrors ShaderPool)
loadShaders files = loadShaderSources files >>= compileShaderSources

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
  print "making program from:"
  print $ vertex shader
  print $ fragment shader
  program <- GL.createProgram
  GL.attachShader program $ vertex shader
  GL.attachShader program $ fragment shader
  GL.linkProgram program
  linkStatusOK <- GL.linkStatus program
  when (not linkStatusOK) $ error "Failed to link program"
  bindProgramData program

bindProgramData :: GL.Program -> IO GLProgram
bindProgramData program = GLProgram <$> pure program
                                    <*> getUniform "time"
                                    <*> getUniform "cameraPosition"
                                    <*> getUniform "cameraRotation"
                                    <*> getUniform "cameraViewMatrics"
                                    <*> getUniform "objectPosition"
                                    <*> getUniform "objectRotation"
                                    <*> getUniform "objectBaseColor"
                                    <*> getUniform "objectScale"
                                   
  where
    getUniform = get . GL.uniformLocation program

