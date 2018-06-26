# pure-transition

Easy-to-use transitions and transition groups for pure.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Pure hiding (Transition,Animation)
import Pure.Transition as T

import Pure.Data.CSS

main = inject body $ flip ComponentIO () $ \self -> 
    let 
        upd = modify_ self . const

        add = upd (\(n,xs) -> (n + 1,xs ++ [n]))
        rem = upd (\(n,xs) -> (n + 1,safeTail xs))

        safeTail [] = []
        safeTail xs = tail xs

        t n = (n,Div <| Width (pxs 90) . Height (pxs 90) . BackgroundColor blue)

    in def
            { construct = return (0,[])
            , render = \_ (_,xs) -> 
                Div <||>
                  [ Button <| OnClick (\_ -> add) |> [ "Add" ]
                  , Button <| OnClick (\_ -> rem) |> [ "Remove" ]
                  , T.Group def <| Animation fadeDown |#> ( map t xs )
                  ]
            }
```
