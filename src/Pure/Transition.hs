-- |
-- Module      : Pure.Transition.Group
-- Description : Transition Group Component
-- Copyright   : (c) Sean Hickman, 2020
-- License     : BSD-3
-- Maintainer  : sean@grumply.com
-- Stability   : experimental
-- Portability : JavaScript
--
module Pure.Transition where

import Pure (Pure(..))
import Pure.Elm hiding (Transition,Left,Right,not,(#),visible,visibility,Hidden,Visible,active,children,features)
import qualified Pure.Elm as Pure
import Pure.Data.Txt as Txt (tail)
import Pure.Data.Cond
import Pure.Data.Prop.TH

import Control.Monad (when)

-- Inspired by Semantic-UI-React's Transition component
-- Modified to work with some type-level machinery
-- and simplified for cleaner code. May lack a few bells
-- and/or whistles for a greatly simplified interface and
-- implementation.

data Status = Entering | Entered | Exiting | Exited 
  deriving (Ord,Eq,Show)

instance Theme Entering
instance Theme Entered
instance Theme Exiting
instance Theme Exited

data Visibility = Hidden | Visible
  deriving (Ord,Eq,Show)

data Interrupted
data Animating
data Disabled

data Transition = Transition_
  { as :: Features -> [View] -> View
  , features :: Features
  , children :: [View] 
  , onStatus :: Status -> IO ()
  , onVisibility :: Visibility -> IO ()
  , interruptible :: Bool
  , active :: Bool
  , looping :: Bool
  , variant :: SomeTheme
  }

deriveLocalComponent ''Transition

instance Default Transition where
  def = 
    Transition_
      { as = \fs cs -> Div & Features fs & Children cs,
        features = mempty, 
        children = mempty, 
        onStatus = const (pure ()),
        onVisibility = const (pure ()),
        interruptible = False,
        active = True, 
        looping = False,
        variant = mkSomeTheme @Transition
      }

data Model = Model 
  { status      :: Status 
  , visibility  :: Visibility
  , awaiting    :: Model -> Model
  , interrupted :: Bool
  }

data Msg = Receive | Complete

-- This is just a big state machine - nothing fancy. 
-- I'm sure I mis-encoded somewhere, though.
instance Pure Transition where
  view t = run (App [] [Receive] [] (pure mdl) update render) t
    where
      mdl | active t    = Model Entering Visible id False
          | otherwise   = Model Exited   Hidden  id False

      update Receive Transition_ {..} Model {..} = do
        -- Ties with the status and awaiting record updates below
        case (active,status) of
          (False,Entering) 
            | interruptible  -> onStatus Exiting
          (False,Entered)    -> onStatus Exiting
          (True,Exiting)   
            | interruptible  -> onStatus Entering
          (True,Exited)      -> onStatus Entering
          _                  -> pure ()

        -- Ties with the visibility record update below
        when (not active && status == Exited) do
          onVisibility Visible

        pure Model
          { visibility =
            case (active,status) of
              (False,Exited) -> Hidden
              _              -> Visible

          , status =
            case (active,status) of
              (False,Entered)   -> Exiting
              (False,Entering) 
                | interruptible -> Exiting
              (True,Exiting)   
                | interruptible -> Entering
              (True,Exited)     -> Entering
              _                 -> status

          , interrupted =
            case (interruptible,active,status) of
              (True,False,Entering) -> True
              (True,True,Exiting)   -> True
              _                     -> False

          , awaiting =
            case (interruptible,active,status) of
              (False,True,Exiting ) -> \m -> m 
                { status     = Entering
                , visibility = Visible 
                , awaiting   = id
                }
              (False,False,Entering) -> \m -> m 
                { status     = Exiting
                , visibility = Visible 
                , awaiting   = id
                }
              _ -> \m -> m 
                { awaiting = id }

          }

      update Complete Transition_ {..} Model {..} = do
        case status of
          Entering -> onStatus Entered
          Exiting  -> onStatus Exited
          _        -> pure ()

        when (status == Exiting) do
          onVisibility Hidden

        pure $ awaiting Model
          { status =
            case status of
              Entering -> Entered
              Exiting  -> Exited
              s        -> s
          , visibility =
            case status of
              Exiting -> Hidden
              _       -> Visible
          , ..
          }

      render :: Elm Msg => Transition -> Model -> View
      render Transition_ {..} Model {..} =
        let
          animatingTheme =
            case status of
              Entering -> Themed @Animating
              Exiting  -> Themed @Animating
              _        -> id

          statusTheme =
            case status of
              Entering  -> Themed @Entering
              Entered   -> Themed @Entered
              Exiting   -> Themed @Exiting
              Exited    -> Themed @Exited

          visibilityTheme =
            case visibility of
              Visible -> Themed @Visible
              Hidden  -> Themed @Hidden

          loopingTheme =
            case looping of
              True -> Themed @Looping
              _    -> id

          interruptedTheme =
            case interrupted of
              True -> Themed @Interrupted
              _    -> id

          statusCallback
            | Entering <- status = On "animationend" (const (command Complete)) 
                                 . On "animationcancel" (const (command Complete))
            | Exiting  <- status = On "animationend" (const (command Complete)) 
                                 . On "animationcancel" (const (command Complete))
            | otherwise          = id

          props = someThemed variant 
                . animatingTheme 
                . statusTheme 
                . visibilityTheme 
                . loopingTheme
                . interruptedTheme
                . statusCallback 

        in
          as (props features) children

instance Theme Transition where
  theme c =
    is c do
      animation-iteration-count        =: 1
      animation-duration               =: 225ms
      animation-timing-function        =: ease
      animation-fill-mode              =: both

instance Theme Animating where
  theme c =
    is c do
      is (subtheme @Transition) do
        backface-Pure.visibility =: hidden

instance Theme Hidden where
  theme c =
    is c do
      is (subtheme @Transition) do
        Pure.visibility      =: hidden
        display              =: none

instance Theme Visible where
  theme c =
    is c do
      is (subtheme @Transition) do
        important $ display    =: block
        important $ Pure.visibility =: Pure.visible

instance Theme Disabled where
  theme c =
    is c do
      is (subtheme @Transition) do
        animation-play-state =: paused

instance Theme Looping where
  theme c =
    is c do
      is (subtheme @Transition) do
        animation-iteration-count =: infinite

instance Theme Interrupted where
  theme c = do
    is c do
      is (subtheme @Transition) do
        is (subtheme @Looping) do
          is (subtheme @Exiting) do
            animation-iteration-count =: 1
        
          is (subtheme @Entering) do
            animation-iteration-count =: 1


