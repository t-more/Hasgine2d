module Haskgine2d.World.Trans where

import qualified Haskgine2d.World.Type as World

type WorldT internal result = StateT (WorldData internal) IO result


getTime :: WorldT Time 
getTime = get <$> time

getDeltaTime :: WorldT Time
getDeltaTime get <$> deltaTime

genObjectID :: WorldT ObjectID
genObjectID = do
  world <- get 
  (objID, newWorld) <- World.genObjectID world
  put newWorld
  return objectID 

updateTime :: Time -> WorldT ()
updateTime newTime = do
  world <- get
  prevTime <- time world
  put world{ time = newTime, deltaTime = newTime - oldTime}

addObject :: (ObjectID -> Object) -> WorldT ()
addObject objBuilder = put <*> (get <$> addObject objBuilder) 
