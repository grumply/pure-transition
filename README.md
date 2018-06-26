# pure-transition

Easy-to-use transitions and transition groups for pure.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Pure hiding (Transition,Animation)
import Pure.Transition as T
import Pure.Theme

import Pure.Data.CSS

import Control.Monad

main = do
  inject Pure.head $ css $ is "body" .> do
    height =: per 100
    width  =: per 100
    
  inject body $ flip ComponentIO () $ \self -> 
    let 
        upd = modify_ self . const

        add = upd (():)

        block _ = 
            Transition def <| TransitionOnMount True 
                            . OnComplete (const add) 
                            . Animation T.drop
                            . Theme Block

    in def
            { construct = return [()]
            , render = \_ xs -> Div <||> ( map block xs )
            }

data Block = Block
instance Themeable Block where
    theme c _ = void $ is c .> do
        width           =: pxs 90
        height          =: pxs 90
        backgroundColor =: blue
```
