{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Convenience where
  
import Graphics.SpriteKit


-- Define a texture based on the image data in the given file, while also
-- determining its width and height.
--
defineTexture :: FilePath -> (Texture, GFloat, GFloat)
defineTexture path = (tex, sizeWidth, sizeHeight)
  where
    tex      = textureWithImageNamed path
    Size{..} = textureSize tex

-- Run an action on the specified child node.
--
runActionOn :: String -> Action children -> SDirective node children
runActionOn childName action 
  = runAction $ runActionOnChildWithName action childName

-- Run a custom action on the specified child node.
--
runCustomActionOn :: String -> TimedUpdate children -> SDirective node children
runCustomActionOn childName actionFun 
  = runActionOn childName $ customAction actionFun

-- Run the given action in an endless loop.
--
runActionForever :: SAction node children -> SDirective node children
runActionForever = runAction . repeatActionForever

-- Run the given sequence of action in an endless loop.
--
runActionSequenceForever :: [SAction n c] -> SDirective n c
runActionSequenceForever = runActionForever . sequenceActions