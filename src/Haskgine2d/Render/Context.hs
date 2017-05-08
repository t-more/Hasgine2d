{- |
Mostly as a not to self,
This module isn't really ment to be used in a working environment.
It should be viewed as a set of methods that can be integrated into a more high level
system.

-}
module Haskgine2d.Render.Context (
  Context,
  ObjectID,
  createContext,
  findProgram,
  addProgramPool,
  getObjectTemplate,
  addObjectTemplate,
  addObject,
  getObject,
  objectFromShape,
  alterObject,
  
  rotateCamera,
  setCameraRotation,
  drawAll  
  ) where

import qualified Haskgine2d.Render.Object as Render
import qualified Haskgine2d.Render.Shaders as Render
import qualified Haskgine2d.Render.Shapes as Render

import Graphics.Rendering.OpenGL (Vector2(..))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Control.Monad.IO.Class

type ObjectID = Int 
data Context = Context {
  objects  :: Map Int Render.Object,
  objectTemplates :: Map String Render.Object,
  defaultProgram :: Render.GLProgram,
  programs :: Render.ProgramPool,
  nextObjID :: ObjectID,

  cameraPosition :: Vector2 Float,
  cameraRotation :: Float
  }


               
createContext :: Render.GLProgram -> Context
createContext defaultProgram = Context mempty mempty defaultProgram mempty 0 (Vector2 0 0) 0

getObjectTemplate :: Context -> String -> Maybe Render.Object
getObjectTemplate context name = Map.lookup name $ objectTemplates context

addObjectTemplate :: Context -> String -> Render.Object -> Context
addObjectTemplate context name  obj = context{objectTemplates = Map.insert name obj (objectTemplates context)}

findProgram :: Context -> String -> Maybe Render.GLProgram
findProgram context programName = Map.lookup programName (programs context) 

genObjID :: Context -> (ObjectID, Context)
genObjID context = (objID, context{nextObjID = succ objID})
  where
    objID = nextObjID context

addProgramPool :: Context -> Render.ProgramPool -> Context
addProgramPool context pool = context{ programs = programs context <> pool}

getObject :: Context -> ObjectID -> Maybe Render.Object
getObject context id = Map.lookup id $ objects context

alterObject :: Context -> ObjectID -> (Render.Object -> Render.Object) -> Context
alterObject context objID alterFunction  = case getObject context objID of
  Nothing -> context
  Just obj -> context{ objects = Map.insert objID (alterFunction obj) (objects context)}

  
addObject :: Context -> Render.Object -> (ObjectID, Context)
addObject context1 obj = (objID, context1{ objects = Map.insert objID obj (objects context2)})
  where
    (objID, context2) = genObjID context1

objectFromShape :: Context -> Render.Shape -> (Render.Object -> Render.Object) -> IO (ObjectID, Context)
objectFromShape context1 shape initiate = do
  obj <- Render.toObject (defaultProgram context1) (Render.vertices shape) (Render.indices shape)
  let (objID, context2) = addObject context1 (initiate obj)
  return (objID, context2)

  
setCameraPosition :: Context -> Vector2 Float -> Context
setCameraPosition context pos = context{ cameraPosition = pos}

moveCamera :: Context -> Vector2 Float -> Context
moveCamera context (Vector2 a b) = context{ cameraPosition = Vector2 (x + a) (y + b)}
  where
    Vector2 x y = cameraPosition context                                    
    
rotateCamera :: Context -> Float -> Context
rotateCamera context rads = context{ cameraRotation = cameraRotation context + rads}

setCameraRotation :: Context -> Float -> Context
setCameraRotation context rads = context{ cameraRotation = rads}




{- |
Draws everything in a context.
-}
drawAll :: Context -> Float -> IO ()
drawAll context time = do
  let drawFull obj = do
        Render.initObjectDraw obj
        Render.setContextUniforms time camPos camRot obj
        Render.drawObject obj

      drawer :: Maybe Render.Object -> Render.Object -> IO ()
      drawer Nothing obj = drawFull obj
      drawer (Just prevObj) obj = if Render.program prevObj == Render.program obj
                                   then Render.drawObject obj
                                   else drawFull obj                                        
  liftIO $ mapM_ (uncurry drawer) (zip (Nothing : map Just elements) elements)
  where
    elements = Map.elems (objects context)
    camPos = cameraPosition context
    camRot = cameraRotation context
    
   
 
