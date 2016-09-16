module Actions where
  
import Graphics.SpriteKit

import GameState


-- The bump used to lift Lambda on key press.
--
bumpAction :: LambdaNode -> TimeInterval -> LambdaNode
bumpAction sprite@Sprite{ nodePhysicsBody = Just body } _dt
  = sprite
    { nodePhysicsBody 
        = Just body
               { bodyVelocity          = vectorZero
               , bodyForcesAndImpulses = [ApplyImpulse (Vector 0 20) Nothing]
               }
    }
bumpAction node _dt = node

-- Tilt Lambda in dependence on its vertical velocity vector.
--
tiltAction :: LambdaNode -> TimeInterval -> LambdaNode
tiltAction sprite@Sprite{ nodePhysicsBody = Just body } _dt
  = sprite
    { nodeZRotation = (-1) `max` zRotation `min` 0.5 }
  where
    zRotation = dY * (if dY < 0 then 0.003 else 0.001 )
    dY        = vectorDy . bodyVelocity $ body
tiltAction node _dt = node
