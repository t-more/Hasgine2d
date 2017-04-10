module Haskgine2d.World.Trans where


import Control.Monad.Trans.State (StateT(..), get, put)

import Haskgine2d.World.Type (World, Time)
import qualified Haskgine2d.World.Type as World

import Haskgine2d.World.Object

type WorldT inner result = StateT (World inner) IO result
type SimpleWorldT result = WorldT () result

getTime :: WorldT a Time 
getTime = World.time <$> get

getDeltaTime :: WorldT a Time
getDeltaTime =  World.deltaTime <$> get

genObjectID :: WorldT a ObjectID
genObjectID = do
  world <- get 
  let (objID, newWorld) = World.genObjectID world
  put newWorld
  return objID

updateTime :: Time -> WorldT a ()
updateTime newTime = World.updateTime newTime <$> get >>= put
  
addObject :: (ObjectID -> Object) -> WorldT a ()
addObject objBuilder = World.addObject objBuilder <$> get >>= put
