-- |
-- Module      : Pure.Transition.Group
-- Description : Transition Group Component
-- Copyright   : (c) Sean Hickman, 2020
-- License     : BSD-3
-- Maintainer  : sean@grumply.com
-- Stability   : experimental
-- Portability : JavaScript
--
module Pure.Transition.Group where

import qualified Pure.Transition as T
import Pure.Transition.Utils

import Pure (Pure(..),HasKeyedChildren(..))
import Pure.Elm hiding (Transition,Left,Right,not,(#),visible,visibility,Hidden,Visible,active,children,features,keyedChildren)
import qualified Pure.Elm as Pure
import Pure.Data.Txt as Txt (tail)
import Pure.Data.Cond
import Pure.Data.Prop.TH

import Control.Monad (when)
import Data.Maybe (fromMaybe,isJust,fromJust)

data Group = Group_
  { as       :: Features -> [(Int,View)] -> View
  , features :: Features
  , withTransition :: T.Transition -> T.Transition
  , keyedChildren :: [(Int,View)]
  }

instance Default Group where
    def = 
      Group_
        { as             = \fs cs -> (Keyed Div) & Features fs & KeyedChildren cs
        , features       = mempty
        , withTransition = id
        , keyedChildren  = mempty
        }

mkComponentPattern ''Group
deriveHasFeatures ''Group
deriveLocalProps ''Group

instance HasKeyedChildren Group where
  getKeyedChildren = keyedChildren
  setKeyedChildren kcs g = g { keyedChildren = kcs }

data GroupState = GroupState
  { buffer :: [(Int,View)] }

-- Something is not quite right here. When transitioning out 
-- an element that gets re-added during the transition, it is
-- not correctly transitioned back in: the element does not 
-- get re-rendered.
instance Pure Group where
  view =
    Component $ \self ->
      let
        handleOnHide key =
          modify_ self $ \_ GroupState {..} -> 
            GroupState 
              { buffer = Prelude.filter ((/= key) . fst) buffer }

        wrapChild f vis (key,child) =
          (key,View $ f $ T.Transition def
                  { T.active        = vis
                  , T.interruptible = False
                  , T.onVisibility  = \case
                    T.Hidden -> handleOnHide key
                    _        -> pure ()
                  , T.children      = [child]
                  }
          )

        hide :: View -> View
        hide (View T.Transition_ {..}) = 
          View T.Transition_ 
            { T.active = False
            , .. 
            }

        fromTransition (Just (View t@T.Transition_ {})) f = Just (f t)
        fromTransition _ _ = Nothing

      in def
          { construct = do
            tg@Group_ {..} <- ask self
            return GroupState
              { buffer = fmap (wrapChild withTransition True) keyedChildren
              }

          , receive = \Group_ { keyedChildren = cs, .. } GroupState {..} -> 
            return GroupState
              { buffer = flip fmap (mergeMappings buffer cs) $ \(k,c) ->
                let 
                  prevChild = lookup k buffer
                  hasPrev   = isJust prevChild
                  hasNext   = isJust (lookup k cs)
                  leaving   = fromMaybe False (fromTransition prevChild (not . T.active))
                  entering  = hasNext && (not hasPrev || leaving)
                  exiting   = not hasNext && hasPrev && not leaving

                in if | entering  -> wrapChild withTransition True (k,c)
                      | exiting   -> (k,hide (fromJust prevChild))
                      | otherwise -> fromJust $ fromTransition prevChild $ \T.Transition_ {..} ->
                                      wrapChild withTransition active (k,c)
              }

          , render = \Group_ {..} GroupState {..} -> as features buffer
          }

