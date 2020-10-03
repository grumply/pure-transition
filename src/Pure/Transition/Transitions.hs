{-|
Module      : Pure.Transition.Transitions
Description : Transition CLasses
Copyright   : (c) Sean Hickman, 2020
License     : BSD-3
Maintainer  : sean@grumply.com
Stability   : experimental
Portability : JavaScript

A bevy of transitions with type-level durations specified in milliseconds.

Transitions from semantic-ui: https://github.com/Semantic-Org/Semantic-UI-CSS/blob/master/comp1nts/transition.css
-}
module Pure.Transition.Transitions where

import Pure.Transition
import Pure.Transition.Utils

import Pure.Elm
import qualified Pure.Data.Txt as Txt (tail)
import GHC.TypeLits

import Data.Proxy (Proxy(..))
import Prelude hiding (or)

-- Good default is `BrowseIn 250`
data BrowseIn (duration :: Nat)
instance KnownNat duration => Theme (BrowseIn duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      -- translate3d forces GPU-acceleration for smoother transitions
      is (0%) do
        apply do
          transform =: scale(0.8) <<>> translate3d(0,0,0)
          z-index   =: (-1)

      is (10%) do
        apply do
          transform =: scale(0.8) <<>> translate3d(0,0,0)
          z-index   =: (-1)

      is (80%) do
        apply do
          transform =: scale(1.05) <<>> translate3d(0,0,0)
          z-index   =: 999
          opacity   =: 0.7

      is (100%) do
        apply do
          transform =: scale(1.05) <<>> translate3d(0,0,0)
          z-index   =: 999
          opacity   =: 1

-- Good default is `BrowseOut 200`
data BrowseOut (duration :: Nat)
instance KnownNat duration => Theme (BrowseOut duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          z-index   =: 999
          transform =: translateX(0%) <<>> rotateY(0deg) <<>> rotateX(0deg)

      is (50%) do
        apply do
          z-index   =: (-1)
          transform =: translateX((-105)%) <<>> rotateY(35deg) <<>> rotateX(10deg) <<>> translateZ((-10)px)

      is (80%) do
        apply do
          opacity =: 1

      is (100%) do 
        apply do
          z-index   =: (-1)
          transform =: translateX(0%) <<>> rotateY(0deg) <<>> rotateX(0deg) <<>> translateZ((-10)px)
          opacity   =: 0

-- Good default is `BrowseOutRight 200`
data BrowseOutRight (duration :: Nat)
instance KnownNat duration => Theme (BrowseOutRight duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          z-index   =: 999
          transform =: translateX(0%) <<>> rotateY(0deg) <<>> rotateX(0deg)

      is (50%) do
        apply do
          z-index   =: 1
          transform =: translateX((-105)%) <<>> rotateY(35deg) <<>> rotateX(10deg) <<>> translateZ((-10)px)

      is (80%) do
        apply do
          opacity =: 1

      is (100%) do
        apply do
          z-index   =: 1
          transform =: translateX(0%) <<>> rotateY(0deg) <<>> rotateX(0deg) <<>> translateZ((-10)px)
          opacity   =: 0

-- Good default is `DropIn 250`
data DropIn (duration :: Nat)
instance KnownNat duration => Theme (DropIn duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            transform-origin          =* [top,center]
            animation-duration        =: t ms
            animation-timing-function =: cubez(0.34,1.61,0.7,1)
            animation-name            =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 0
          transform =: scale(0)

      is (100%) do
        apply do
          opacity   =: 1
          transform =: scale(1)

-- Good default is `DropOut 200`
data DropOut (duration :: Nat)
instance KnownNat duration => Theme (DropOut duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            transform-origin          =* [top,center]
            animation-duration        =: t ms
            animation-timing-function =: cubez(0.34,1.61,0.7,1)
            animation-name            =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 1
          transform =: scale(1)

      is (100%) do
        apply do
          opacity   =: 0
          transform =: scale(0)

-- Good default is `FadeIn 250`
data FadeIn (duration :: Nat)
instance KnownNat duration => Theme (FadeIn duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0

      is (100%) do
        apply do
          opacity =: 1

-- Good default is `FadeOut 200`
data FadeOut (duration :: Nat)
instance KnownNat duration => Theme (FadeOut duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
          
    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 1
        
      is (100%) do
        apply do
          opacity =: 0

-- Good default is `FadeInUp 10 250`
data FadeInUp (delta :: Nat) (duration :: Nat)
instance (KnownNat delta, KnownNat duration) => Theme (FadeInUp delta duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        d = fromIntegral (natVal (Proxy :: Proxy delta))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: translateY(d%)

      is (100%) do
        apply do
          opacity =: 1
          transform =: translateY(0%)

-- Good default is `FadeOutUp 5 200`
data FadeOutUp (delta :: Nat) (duration :: Nat)
instance (KnownNat delta, KnownNat duration) => Theme (FadeOutUp delta duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        d = fromIntegral (natVal (Proxy :: Proxy delta))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 1
          transform =: translateY(0%)

      is (100%) do
        apply do
          opacity =: 0
          transform =: translateY(d%)

-- Good default is `FadeInDown 10 250`
data FadeInDown (delta :: Nat) (duration :: Nat)
instance (KnownNat delta, KnownNat duration) => Theme (FadeInDown delta duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        d = fromIntegral (natVal (Proxy :: Proxy delta))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: translateY(negate(d%))

      is (100%) do
        apply do
          opacity =: 1
          transform =: translateY(0%)

-- Good default is `FadeOutDown 5 200`
data FadeOutDown (delta :: Nat) (duration :: Nat)
instance (KnownNat delta, KnownNat duration) => Theme (FadeOutDown delta duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        d = fromIntegral (natVal (Proxy :: Proxy delta))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 1
          transform =: translateY(0%)

      is (100%) do
        apply do
          opacity =: 0
          transform =: translateY(negate(d%))

-- Good default is `FadeInLeft 10 250`
data FadeInLeft (delta :: Nat) (duration :: Nat)
instance (KnownNat delta, KnownNat duration) => Theme (FadeInLeft delta duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        d = fromIntegral (natVal (Proxy :: Proxy delta))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: translateX(negate(d%))

      is (100%) do
        apply do
          opacity =: 1
          transform =: translateX(0%)

-- Good default is `FadeOutLeft 5 200`
data FadeOutLeft (delta :: Nat) (duration :: Nat)
instance (KnownNat delta, KnownNat duration) => Theme (FadeOutLeft delta duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        d = fromIntegral (natVal (Proxy :: Proxy delta))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 1
          transform =: translateX(0%)

      is (100%) do
        apply do
          opacity =: 0
          transform =: translateX(d%)

-- Good default is `FadeInRight 10 250`
data FadeInRight (delta :: Nat) (duration :: Nat)
instance (KnownNat delta, KnownNat duration) => Theme (FadeInRight delta duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        d = fromIntegral (natVal (Proxy :: Proxy delta))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: translateX(negate(d%))

      is (100%) do
        apply do
          opacity =: 1
          transform =: translateX(0%)

-- Good default is `FadeOutRight 5 200`
data FadeOutRight (delta :: Nat) (duration :: Nat)
instance (KnownNat delta, KnownNat duration) => Theme (FadeOutRight delta duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        d = fromIntegral (natVal (Proxy :: Proxy delta))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 1
          transform =: translateX(0%)

      is (100%) do
        apply do
          opacity =: 0
          transform =: translateX(negate(d%))

-- Good default is `HorizontalFlipIn 250`
data HorizontalFlipIn (duration :: Nat)
instance KnownNat duration => Theme (HorizontalFlipIn duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: persp(2000px) <<>> rotY((-90)deg)

      is (100%) do
        apply do
          opacity =: 1
          transform =: persp(2000px) <<>> rotY(0deg)

-- Good default is `HorizontalFlipOut 200`
data HorizontalFlipOut (duration :: Nat)
instance KnownNat duration => Theme (HorizontalFlipOut duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 1
          transform =: persp(2000px) <<>> rotY(0deg)

      is (100%) do
        apply do
          opacity =: 0
          transform =: persp(2000px) <<>> rotY(90deg)

-- Good default is `VerticalFlipIn 250`
data VerticalFlipIn (duration :: Nat)
instance KnownNat duration => Theme (VerticalFlipIn duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: persp(2000px) <<>> rotX((-90)deg)

      is (100%) do
        apply do
          opacity =: 1
          transform =: persp(2000px) <<>> rotX(0deg)

-- Good default is `VerticalFlipOut 200`
data VerticalFlipOut (duration :: Nat)
instance KnownNat duration => Theme (VerticalFlipOut duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 1
          transform =: persp(2000px) <<>> rotX(0deg)

      is (100%) do
        apply do
          opacity =: 0
          transform =: persp(2000px) <<>> rotX(90deg)

-- Good default is `ScaleIn 80 250`
data ScaleIn (scalePercent :: Nat) (duration :: Nat)
instance (KnownNat scalePercent, KnownNat duration) => Theme (ScaleIn scalePercent duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
        s = realToFrac (fromIntegral (natVal (Proxy :: Proxy scalePercent)) / 100 :: Double)
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: scale(s)

      is (100%) do
        apply do
          opacity =: 1
          transform =: scale(1)

-- Good default is `ScaleOut 90 200`
data ScaleOut (scalePercent :: Nat) (duration :: Nat)
instance (KnownNat scalePercent, KnownNat duration) => Theme (ScaleOut scalePercent duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        s = realToFrac (fromIntegral (natVal (Proxy :: Proxy scalePercent)) / 100 :: Double)
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 1
          transform =: scale(1)

      is (100%) do
        apply do
          opacity =: 0
          transform =: scale(s)

-- Good default is `FlyIn 600`
data FlyIn (duration :: Nat)
instance KnownNat duration => Theme (FlyIn duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: scale3d(0.3,0.3,0.3)

      is (20%) do
        apply do
          transform =: scale3d(1.1,1.1,1.1)

      is (40%) do
        apply do
          transform =: scale3d(0.9,0.9,0.9)

      is (60%) do 
        apply do
          opacity =: 1
          transform =: scale3d(1.03,1.03,1.03)

      is (80%) do
        apply do
          transform =: scale3d(0.97,0.97,0.97)

      is (100%) do
        apply do
          opacity =: 1
          transform =: scale3d(1,1,1)

-- Good default is `FlyOut 600`
data FlyOut (duration :: Nat)
instance KnownNat duration => Theme (FlyOut duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (20%) do
        apply do
          transform =: scale3d(0.9,0.9,0.9)

      is (50%) . or is (55%) $ do
        apply do
          opacity =: 1
          transform =: scale3d(1.1,1.1,1.1)

      is (100%) do
        apply do
          opacity =: 0
          transform =: scale3d(0.3,0.3,0.3)

-- Good default is `FlyInUp 600`
data FlyInUp (duration :: Nat)
instance KnownNat duration => Theme (FlyInUp duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: translate3d(0,1500px,0)

      is (60%) do
        apply do
          opacity =: 1
          transform =: translate3d(0,(-20)px,0)

      is (75%) do
        apply do
          transform =: translate3d(0,10px,0)

      is (90%) do
        apply do
          transform =: translate3d(0,(-5)px,0)

      is (100%) do
        apply do
          transform =: translate3d(0,0,0)

-- Good default is `FlyOutUp 600`
data FlyOutUp (duration :: Nat)
instance KnownNat duration => Theme (FlyOutUp duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (20%) do
        apply do
          transform =: translate3d(0,10px,0)

      is (40%) . or is (45%) $ do
        apply do
          opacity   =: 1
          transform =: translate3d(0,(-20)px,0);

      is (100%) do
        apply do
          opacity   =: 0
          transform =: translate3d(0,2000px,0);

-- Good default is `FlyInDown 600`
data FlyInDown (duration :: Nat)
instance KnownNat duration => Theme (FlyInDown duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: translate3d(0,(-1500)px,0)

      is (60%) do
        apply do
          opacity =: 1
          transform =: translate3d(0,25px,0)

      is (75%) do
        apply do
          transform =: translate3d(0,(-10)px,0)

      is (90%) do
        apply do
          transform =: translate3d(0,5px,0)

      is (100%) do
        apply do
          transform =: none

-- Good default is `FlyOutDown 600`
data FlyOutDown (duration :: Nat)
instance KnownNat duration => Theme (FlyOutDown duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (20%) do
        apply do
          transform =: translate3d(0,(-10)px,0)

      is (40%) . is (45%) $ do
        apply do
          opacity =: 1
          transform =: translate3d(0,20px,0)

      is (100%) do
        apply do
          opacity =: 0
          transform =: translate3d(0,(-2000)px,0)

-- Good default is `FlyInLeft 600`
data FlyInLeft (duration :: Nat)
instance KnownNat duration => Theme (FlyInLeft duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: translate3d(1500px,0,0)

      is (60%) do
        apply do
          opacity =: 1
          transform =: translate3d((-25)px,0,0)

      is (75%) do
        apply do
          transform =: translate3d(10px,0,0)

      is (90%) do
        apply do
          transform =: translate3d((-5)px,0,0)

      is (100%) do
        apply do
          transform =: none

-- Good default is `FlyOutLeft 600`
data FlyOutLeft (duration :: Nat)
instance KnownNat duration => Theme (FlyOutLeft duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (20%) do
        apply do
          opacity =: 1
          transform =: translate3d((-20)px,0,0)

      is (100%) do
        apply do
          opacity =: 0
          transform =: translate3d(2000px,0,0)

-- Good default is `FlyInRight 600`
data FlyInRight (duration :: Nat)
instance KnownNat duration => Theme (FlyInRight duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (0%) do
        apply do
          opacity =: 0
          transform =: translate3d((-1500)px,0,0)
      
      is (60%) do
        apply do
          opacity =: 1
          transform =: translate3d(25px,0,0)
      
      is (75%) do
        apply do
          transform =: translate3d((-10)px,0,0)

      is (90%) do
        apply do
          transform =: translate3d(5px,0,0)

      is (100%) do
        apply do
          transform =: none

-- Good default is `FlyOutRight 600`
data FlyOutRight (duration :: Nat)
instance KnownNat duration => Theme (FlyOutRight duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transition-timing-function =: cubez(0.215,0.16,0.355,1)

    atKeyframes nm do

      is (20%) do
        apply do
          opacity =: 1
          transform =: translate3d(20px,0,0)

      is (100%) do
        apply do
          opacity =: 0
          transform =: translate3d((-2000)px,0,0)

-- Good default is `SlideInDown 250`
data SlideInDown (duration :: Nat)
instance KnownNat duration => Theme (SlideInDown duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [top,center]

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 0
          transform =: scaleY(0)

      is (100%) do
        apply do
          opacity   =: 1
          transform =: scaleY(1)

-- Good default is `SlideOutDown 200`
data SlideOutDown (duration :: Nat) 
instance KnownNat duration => Theme (SlideOutDown duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [top,center]

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 1
          transform =: scaleY(1)

      is (100%) do
        apply do
          opacity   =: 0
          transform =: scaleY(0)

-- Good default is `SlideInUp 250`
data SlideInUp (duration :: Nat)
instance KnownNat duration => Theme (SlideInUp duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [bottom,center]

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 0
          transform =: scaleY(0)

      is (100%) do
        apply do
          opacity   =: 1
          transform =: scaleY(1)

-- Good default is `SlideOutUp 200`
data SlideOutUp (duration :: Nat) 
instance KnownNat duration => Theme (SlideOutUp duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [bottom,center]

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 1
          transform =: scaleY(1)

      is (100%) do
        apply do
          opacity   =: 0
          transform =: scaleY(0)

-- Good default is `SlideInLeft 250`
data SlideInLeft (duration :: Nat)
instance KnownNat duration => Theme (SlideInLeft duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [left,center]

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 0
          transform =: scaleX(0)

      is (100%) do
        apply do
          opacity   =: 1
          transform =: scaleX(1)

-- Good default is `SlideOutLeft 200`
data SlideOutLeft (duration :: Nat) 
instance KnownNat duration => Theme (SlideOutLeft duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [left,center]

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 1
          transform =: scaleX(1)

      is (100%) do
        apply do
          opacity   =: 0
          transform =: scaleX(0)

-- Good default is `SlideInRight 250`
data SlideInRight (duration :: Nat)
instance KnownNat duration => Theme (SlideInRight duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [right,center]

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 0
          transform =: scaleX(0)

      is (100%) do
        apply do
          opacity   =: 1
          transform =: scaleX(1)

-- Good default is `SlideOutRight 200`
data SlideOutRight (duration :: Nat) 
instance KnownNat duration => Theme (SlideOutRight duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do

          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [right,center]

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 1
          transform =: scaleX(1)

      is (100%) do
        apply do
          opacity   =: 0
          transform =: scaleX(0)

-- Good default is `SwingInDown 600`
data SwingInDown (duration :: Nat)
instance KnownNat duration => Theme (SwingInDown duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [top,center]

    atKeyframes nm do
      is (0%) do
        apply do
          opacity  =: 0
          transform =: persp(1000px) <<>> rotateX(90deg)

      is (40%) do
        apply do
          opacity =: 1
          transform =: persp(1000px) <<>> rotateX((-30)deg)

      is (60%) do
        apply do
          transform =: persp(1000px) <<>> rotateX(15deg)

      is (80%) do
        apply do
          transform =: persp(1000px) <<>> rotateX((-7.5)deg)

      is (100%) do
        apply do
          transform =: persp(1000px) <<>> rotateX(0deg)

-- Good default is `SwingOutDown 600`
data SwingOutDown (duration :: Nat)
instance KnownNat duration => Theme (SwingOutDown duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [top,center]

    atKeyframes nm do

      is (0%) do
        apply do
          transform =: persp(1000px) <<>> rotateX(0deg)
      
      is (40%) do
        apply do
          transform =: persp(1000px) <<>> rotateX(7.5deg)

      is (60%) do
        apply do
          transform =: persp(1000px) <<>> rotateX(15deg)

      is (80%) do
        apply do
          opacity   =: 1
          transform =: persp(1000px) <<>> rotateX((-30)deg)

      is (100%) do
        apply do
          opacity   =: 0
          transform =: persp(1000px) <<>> rotateX(90deg)

-- Good default is `SwingInUp 600`
data SwingInUp (duration :: Nat)
instance KnownNat duration => Theme (SwingInUp duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [bottom,center]

    atKeyframes nm do
    
      is (0%) do
        apply do
          opacity  =: 0
          transform =: persp(1000px) <<>> rotateX((-90)deg)

      is (40%) do
        apply do
          opacity =: 1
          transform =: persp(1000px) <<>> rotateX(30deg)

      is (60%) do
        apply do
          transform =: persp(1000px) <<>> rotateX((-15)deg)

      is (80%) do
        apply do
          transform =: persp(1000px) <<>> rotateX((7.5)deg)

      is (100%) do
        apply do
          transform =: persp(1000px) <<>> rotateX(0deg)

-- Good default is `SwingOutUp 600`
data SwingOutUp (duration :: Nat)
instance KnownNat duration => Theme (SwingOutUp duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [bottom,center]

    atKeyframes nm do

      is (0%) do
        apply do
          transform =: persp(1000px) <<>> rotateX(0deg)
      
      is (40%) do
        apply do
          transform =: persp(1000px) <<>> rotateX((-7.5)deg)

      is (60%) do
        apply do
          transform =: persp(1000px) <<>> rotateX(15deg)

      is (80%) do
        apply do
          opacity   =: 1
          transform =: persp(1000px) <<>> rotateX((-30)deg)

      is (100%) do
        apply do
          opacity   =: 0
          transform =: persp(1000px) <<>> rotateX(90deg)

-- Good default is `SwingInLeft 600`
data SwingInLeft (duration :: Nat)
instance KnownNat duration => Theme (SwingInLeft duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [center,right]

    atKeyframes nm do
    
      is (0%) do
        apply do
          opacity  =: 0
          transform =: persp(1000px) <<>> rotateY((-90)deg)

      is (40%) do
        apply do
          opacity =: 1
          transform =: persp(1000px) <<>> rotateY(30deg)

      is (60%) do
        apply do
          transform =: persp(1000px) <<>> rotateY((-15)deg)

      is (80%) do
        apply do
          transform =: persp(1000px) <<>> rotateY((7.5)deg)

      is (100%) do
        apply do
          transform =: persp(1000px) <<>> rotateY(0deg)

-- Good default is `SwingOutLeft 600`
data SwingOutLeft (duration :: Nat)
instance KnownNat duration => Theme (SwingOutLeft duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [center,right]

    atKeyframes nm do

      is (0%) do
        apply do
          transform =: persp(1000px) <<>> rotateY(0deg)

      is (40%) do
        apply do
          transform =: persp(1000px) <<>> rotateY(7.5deg)

      is (60%) do
        apply do
          transform =: persp(1000px) <<>> rotateY((-15)deg)

      is (80%) do
        apply do
          opacity   =: 1
          transform =: persp(1000px) <<>> rotateY(30deg)

      is (100%) do
        apply do
          opacity   =: 0
          transform =: persp(1000px) <<>> rotateY((-90)deg)

-- Good default is `SwingInRight 600`
data SwingInRight (duration :: Nat)
instance KnownNat duration => Theme (SwingInRight duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [center,left]

    atKeyframes nm do

      is (0%) do
        apply do
          opacity  =: 0
          transform =: persp(1000px) <<>> rotateY((-90)deg)

      is (40%) do
        apply do
          opacity =: 1
          transform =: persp(1000px) <<>> rotateY(30deg)

      is (60%) do
        apply do
          transform =: persp(1000px) <<>> rotateY((-15)deg)

      is (80%) do
        apply do
          transform =: persp(1000px) <<>> rotateY(7.5deg)

      is (100%) do
        apply do
          transform =: persp(1000px) <<>> rotateY(0deg)
              
-- Good default is `SwingOutRight 600`
data SwingOutRight (duration :: Nat)
instance KnownNat duration => Theme (SwingOutRight duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity,transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            transform-origin =* [center,left]

    atKeyframes nm do

      is (0%) do
        apply do
          transform =: persp(1000px) <<>> rotateY(0deg)

      is (40%) do
        apply do
          transform =: persp(1000px) <<>> rotateY(7.5deg)

      is (60%) do
        apply do
          transform =: persp(1000px) <<>> rotateY((-15)deg)

      is (80%) do
        apply do
          opacity   =: 1
          transform =: persp(1000px) <<>> rotateY(30deg)

      is (100%) do
        apply do
          opacity   =: 0
          transform =: persp(1000px) <<>> rotateY((-90)deg)

-- Good default is `ZoomIn 250`
data ZoomIn (duration :: Nat)
instance KnownNat duration => Theme (ZoomIn duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 1
          transform =: scale(0)

      is (100%) do
        apply do
          opacity   =: 1
          transform =: scale(1)

-- Good default is `ZoomOut 200`
data ZoomOut (duration :: Nat)
instance KnownNat duration => Theme (ZoomOut duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [transform]

        is (subtheme @Exiting) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) do
        apply do
          opacity   =: 1
          transform =: scale(1)

      is (100%) do
        apply do
          opacity   =: 1
          transform =: scale(0)


-- Good default is `Flash 750`
data Flash (duration :: Nat)
instance KnownNat duration => Theme (Flash duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [opacity]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      is (0%) . or is (50%) . or is (100%) $ do
        apply do
          opacity =: 1

      is (25%) . or is (75%) $ do
        apply do
          opacity =: 0

-- Good default is `Shake 750`
data Shake (duration :: Nat)
instance KnownNat duration => Theme (Shake duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
          
    atKeyframes nm do

      is (0%) . or is (100%) $ do
        apply do
          transform =: translateX(0)

      is (10%) . or is (30%) . or is (50%) . or is (70%) . or is (90%) $ do
        apply do
          transform =: translateX((-10)px)

      is (20%) . or is (40%) . or is (60%) . or is (80%) $ do
        apply do
          transform =: translateX(10px)

-- Good default is `Bounce 750`
data Bounce (duration :: Nat)
instance KnownNat duration => Theme (Bounce duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
          
    atKeyframes nm do

      is (0%) . or is (20%) . or is (50%) . or is (80%) . or is (100%) $ do
        apply do
          transform =: translateY(0)

      is (40%) do
        apply do
          transform =: translateY((-30)px)

      is (60%) do
        apply do
          transform =: translateY(15px)

-- Good default is `Tada 750`
data Tada (duration :: Nat)
instance KnownNat duration => Theme (Tada duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
          
    atKeyframes nm do

      is (0%) do
        apply do
          transform =: scale(1)

      is (10%) . or is (20%) $ do
        apply do
          transform =: scale(0.9) <<>> rotate((-3)deg)

      is (30%) . or is (50%) . or is (70%) . or is (90%) $ do
        apply do
          transform =: scale(1.1) <<>> rotate(3deg)

      is (40%) . or is (60%) . or is (80%) $ do
        apply do
          transform =: scale(1.1) <<>> rotate((-3)deg)

      is (100%) do
        apply do
          transform =: scale(1) <<>> rotate(0)

-- Good default is `Pulse 750`
data Pulse (duration :: Nat)
instance KnownNat duration => Theme (Pulse duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
 
    atKeyframes nm do

      is (0%) do
        apply do
          transform =: scale(1)
          opacity   =: 1

      is (50%) do
        apply do
          transform =: scale(0.9)
          opacity   =: 0.7

      is (100%) do
        apply do
          transform =: scale(1)
          opacity   =: 1

-- Good default is `Pulse 750`
data Jiggle (duration :: Nat)
instance KnownNat duration => Theme (Jiggle duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm

    atKeyframes nm do

      let fs = [ (0,1,1,1), (30,1.25,0.75,1), (40,0.75,1.25,1), (50,1.15,0.85,1), (65,0.95,1.05,1), (75,1.05,0.95,1), (100,1,1,1) ]
      for_ fs $ \(p,x,y,z) -> 
        is (p%) do
          apply do 
            transform =: scale3d(x,y,z) 

-- Good default is `Glow 2000`
data Glow (duration :: Nat)
instance KnownNat duration => Theme (Glow duration) where
  theme c = do
    let t = fromIntegral (natVal (Proxy :: Proxy duration))
        nm = Txt.tail c
    is c do
      is (subtheme @Transition) do
        apply do
          will-change =: elems [transform]

        is (subtheme @Entering) do
          apply do
            animation-duration =: t ms
            animation-name =: nm
            animation-timing-function =: cubez(0.19,1,0.22,1)

    atKeyframes nm do

      is (0%) do
        apply do 
          background-color =: hex 0xFCFCFD

      is (30%) do
        apply do
          background-color =: hex 0xFFF6CD

      is (100%) do
        apply do
          background-color =: hex 0xFCFCFD
