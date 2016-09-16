module Constants where

import Graphics.SpriteKit

import Data.Bits
import Data.Word


-- Scene dimensions
--
width, height :: GFloat
width  = 667
height = 750

-- The size of the gap to fly through.
--
verticalPipeGap :: GFloat
verticalPipeGap = 150

-- Categorisation of the various items in the physics world.
--
data PhysicsCategory = Bird       -- Lambda
                     | World      -- Pipes & the ground
                     | Score      -- Scoring nodes
                     deriving (Enum)

categoryBitMask :: [PhysicsCategory] -> Word32
categoryBitMask = foldl setCategoryBit zeroBits
  where
    setCategoryBit bits cat = bits .|. bit (fromEnum cat)

isInCategory :: PhysicsCategory -> Node u -> Bool
isInCategory cat node
  = case nodePhysicsBody node of
      Just body -> testBit (bodyCategoryBitMask body) (fromEnum cat)
      Nothing   -> False

isWorld :: Node u -> Bool
isWorld = isInCategory World

isScore :: Node u -> Bool
isScore = isInCategory Score

-- Background colour of the sky
--
skyColour :: Color
skyColour = colorWithRGBA (81.0/255.0) 
                          (192.0/255.0)
                          (201.0/255.0)
                          1.0
