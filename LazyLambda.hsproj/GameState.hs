module GameState where

import Graphics.SpriteKit

  
data NodeState = PipesState [Int]      -- unbound sequence of random numbers
               | NoState

type LambdaNode = Node NodeState

randomInt :: NodeState -> (NodeState, Int)
randomInt NoState             = (NoState, 0)
randomInt (PipesState (i:is)) = (PipesState is, i)


data GameState = Running | Crash | Over
               deriving Eq

data SceneState = SceneState 
                 { sceneScore :: Int
                 , keyPressed :: Bool
                 , bumpScore  :: Bool
                 , gameState  :: GameState
                 }

initialSceneState 
  = SceneState 
    { sceneScore = 0
    , keyPressed = False 
    , bumpScore  = False 
    , gameState  = Running
    }

type LambdaScene = Scene SceneState NodeState
