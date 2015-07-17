module Engine.Input where

data Input = Input
    { pressedKeys :: PressedKeys
    }

data PressedKeys = PressedKeys
    { down :: Bool
    , up :: Bool
    , left :: Bool
    , right :: Bool
    }

pressedKeysDefault :: PressedKeys
pressedKeysDefault = PressedKeys
    { down = False
    , up = False
    , left = False
    , right = False
    }
