{-# LANGUAGE OverloadedStrings #-}
module Pure.Transition.Utils where

import Control.Applicative
import Control.Monad.ST
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.STRef

import Pure.Data.CSS

import Pure.Data.Styles

-- This is hideous; functionalize.
{-# INLINE mergeMappings #-}
mergeMappings :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeMappings prev next = runST $ do
    childMapping    <- newSTRef []
    nextKeysPending <- newSTRef []
    pendingKeys     <- newSTRef []

    let swap ref ys = do
            xs <- readSTRef ref
            writeSTRef ref ys
            return xs

    for_ prev $ \(prevKey,_) -> do
        case lookup prevKey next of
            Just _ -> do
                pks <- swap pendingKeys []
                modifySTRef' nextKeysPending ((prevKey,Prelude.reverse pks):)

            Nothing ->
                modifySTRef' pendingKeys (prevKey:)

    let value k = fromJust (lookup k next <|> lookup k prev)
        addChildMapping k = modifySTRef' childMapping ((:) (k,value k))

    nkps <- readSTRef nextKeysPending
    for_ next $ \(nextKey,_) -> do
        for_ (lookup nextKey nkps)
            (traverse_ addChildMapping)
        addChildMapping nextKey

    pks <- readSTRef pendingKeys
    for_ (Prelude.reverse pks) addChildMapping

    Prelude.reverse <$> readSTRef childMapping

persp p = "perspective(" <> p <> ")"
rotY ry = "rotateY(" <> ry <> ")"
rotX rx = "rotateX(" <> rx <> ")"

animIter n = do
    "-webkit-animation-iteration-count" =: n
    "animation-iteration-count"        =: n

animName n = do
    "-webkit-animation-name" =: n
    "animation-name"        =: n

animDur t = do
    "-webkit-animation-duration" =: ms t
    "animation-duration"        =: ms t

keyframes nm kfs = do
    atWebkitKeyframes (" " <> nm <> " ") kfs
    atKeyframes (" " <> nm <> " ") kfs

trans t = do
    "-webkit-transform" =: t
    "transform" =: t

transOrigin to = do
    "-webkit-transform-origin" =: to
    "transform-origin" =: to

animTiming t = do
    "-webkit-animation-timing-function" =: cubicBezier(t)
    "animation-timing-function" =: cubicBezier(t)
