module Pure.Transition.Utils where

import Pure.Elm

import Control.Applicative
import Control.Monad.ST
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.STRef
 

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

