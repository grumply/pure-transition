{-# LANGUAGE PatternSynonyms, ViewPatterns, TypeFamilies, KindSignatures, OverloadedStrings, MultiWayIf, ExistentialQuantification, DuplicateRecordFields, RecordWildCards, MultiParamTypeClasses, DeriveGeneric, DeriveAnyClass #-}
module Pure.Transition where

-- from pure
import Pure hiding (Transition,animation,visible)

-- from pure-cond
import Pure.Data.Cond

-- from pure-css
import Pure.Data.CSS

-- from pure-theme
import Pure.Theme

-- from pure-styles
import Pure.Data.Styles hiding (visible,animation)

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Monad (void)
import Data.Foldable (for_)
import Data.IORef
import Data.Maybe
import Data.Monoid
import GHC.Generics as G

import Data.Function as Tools ((&))

import Pure.Transition.Utils

class HasProp p a where
    type Prop p a :: *
    getProp :: p -> a -> Prop p a
    setProp :: p -> Prop p a -> a -> a

data As = As_
pattern As :: HasProp As a => Prop As a -> a -> a
pattern As p a <- (getProp As_ &&& id -> (p,a)) where
    As p a = setProp As_ p a

data Animation = Animation_
pattern Animation :: HasProp Animation a => Prop Animation a -> a -> a
pattern Animation p a <- (getProp Animation_ &&& id -> (p,a)) where
    Animation p a = setProp Animation_ p a

data AnimationDuration
    = Uniform Int
    | Skewed
        { hide :: Int
        , show :: Int
        }
    deriving (Generic,Default,Ord,Eq)

pattern AnimationDuration :: HasProp AnimationDuration a => Prop AnimationDuration a -> a -> a
pattern AnimationDuration p a <- (getProp (undefined :: AnimationDuration) &&& id -> (p,a)) where
    AnimationDuration p a = setProp (undefined :: AnimationDuration) p a

data Visible = Visible_
pattern Visible :: HasProp Visible a => Prop Visible a -> a -> a
pattern Visible p a <- (getProp Visible_ &&& id -> (p,a)) where
    Visible p a = setProp Visible_ p a

data MountOnShow = MountOnShow_
pattern MountOnShow :: HasProp MountOnShow a => Prop MountOnShow a -> a -> a
pattern MountOnShow p a <- (getProp MountOnShow_ &&& id -> (p,a)) where
    MountOnShow p a = setProp MountOnShow_ p a

data OnComplete = OnComplete_
pattern OnComplete :: HasProp OnComplete a => Prop OnComplete a -> a -> a
pattern OnComplete p a <- (getProp OnComplete_ &&& id -> (p,a)) where
    OnComplete p a = setProp OnComplete_ p a

data OnHide = OnHide_
pattern OnHide :: HasProp OnHide a => Prop OnHide a -> a -> a
pattern OnHide p a <- (getProp OnHide_ &&& id -> (p,a)) where
    OnHide p a = setProp OnHide_ p a

data OnShow = OnShow_
pattern OnShow :: HasProp OnShow a => Prop OnShow a -> a -> a
pattern OnShow p a <- (getProp OnShow_ &&& id -> (p,a)) where
    OnShow p a = setProp OnShow_ p a

data OnStart = OnStart_
pattern OnStart :: HasProp OnStart a => Prop OnStart a -> a -> a
pattern OnStart p a <- (getProp OnStart_ &&& id -> (p,a)) where
    OnStart p a = setProp OnStart_ p a

data TransitionOnMount = TransitionOnMount_
pattern TransitionOnMount :: HasProp TransitionOnMount a => Prop TransitionOnMount a -> a -> a
pattern TransitionOnMount p a <- (getProp TransitionOnMount_ &&& id -> (p,a)) where
    TransitionOnMount p a = setProp TransitionOnMount_ p a

data UnmountOnHide = UnmountOnHide_
pattern UnmountOnHide :: HasProp UnmountOnHide a => Prop UnmountOnHide a -> a -> a
pattern UnmountOnHide p a <- (getProp UnmountOnHide_ &&& id -> (p,a)) where
    UnmountOnHide p a = setProp UnmountOnHide_ p a

data TransitionStatus = Unmounted | Entered | Entering | Exited | Exiting
    deriving (Generic,Default,Ord,Eq)

calculateTransitionDuration :: TransitionStatus -> AnimationDuration -> Int
calculateTransitionDuration _        (Uniform d) = d
calculateTransitionDuration Exiting  Skewed {..} = hide
calculateTransitionDuration _        Skewed {..} = show

data Transition = Transition_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , animation :: SomeTheme
    , duration :: AnimationDuration
    , visible :: Bool
    , mountOnShow :: Bool
    , onComplete :: TransitionStatus -> IO ()
    , onHide :: TransitionStatus -> IO ()
    , onShow :: TransitionStatus -> IO ()
    , onStart :: TransitionStatus -> IO ()
    , transitionOnMount :: Bool
    , unmountOnHide :: Bool
    } deriving (Generic)

instance Default Transition where
    def = (G.to gdef)
        { as = \fs cs -> Div & Features fs & Children cs
        , animation = fade
        , duration = Uniform 500
        , visible = True
        , mountOnShow = True
        }

pattern Transition :: Transition -> Transition
pattern Transition t = t

data TransitionState = TS
    { status :: TransitionStatus
    , animating :: Bool
    , mounted :: IORef Bool
    , next :: IORef (Maybe TransitionStatus)
    , transitionTimeout :: IORef (Maybe ThreadId)
    }

instance Pure Transition where
    view =
        LibraryComponentIO $ \self ->
            let

                setSafeState f = do
                    TS {..} <- get self
                    mtd <- readIORef mounted
                    mtd # modifyM_ self (const f)

                handleStart = do
                    Transition_ {..} <- ask self
                    TS          {..} <- get self

                    upcoming <- readIORef next
                    writeIORef next def

                    setSafeState $ \TS {..} -> return
                        ( TS { status = fromMaybe status upcoming
                             , animating = True
                             , ..
                             }
                        , for_ upcoming $ \s -> do
                            onStart s
                            tid <- forkIO $ do
                                threadDelay (calculateTransitionDuration s duration * 1000)
                                handleComplete
                            writeIORef transitionTimeout (Just tid)
                        )

                handleComplete = do
                    Transition_ {..} <- ask self
                    TS          {..} <- get self
                    onComplete status
                    maybe (return ()) (const handleStart) =<< readIORef next
                    s <- computeCompletedStatus
                    let callback = (status == Entering) ? onShow $ onHide
                    setSafeState $ \TS {..} -> return
                        ( TS { status = s, animating = False, .. }
                        , callback s
                        )

                updateStatus = do
                    Transition_ {..} <- ask self
                    TS          {..} <- get self
                    upcoming <- readIORef next
                    (isJust upcoming) # do
                        writeIORef next . Just =<< computeNextStatus
                        (not animating) # handleStart

                computeCompletedStatus = do
                    Transition_ {..} <- ask self
                    TS          {..} <- get self

                    return $
                      (status == Entering)
                        ? Entered
                        $ unmountOnHide
                            ? Unmounted
                            $ Exited

                computeInitialStatuses = do
                    Transition_ {..} <- ask self
                    return $
                        if | visible && transitionOnMount -> (Exited   ,Just Entering)
                           | visible                      -> (Entered  ,Nothing)
                           | mountOnShow || unmountOnHide -> (Unmounted,Nothing)
                           | otherwise                    -> (Exited   ,Nothing)

                computeNextStatus = do
                    TS {..} <- get self
                    return $
                        if animating
                            then (status == Entering) ? Exiting $ Entering
                            else (status == Entered)  ? Exiting $ Entering

                computeStatuses True status =
                    ( (status == Unmounted) ? Just Exited $ Nothing
                    , (status /= Entering && status /= Entered) ? Just Entering $ Nothing
                    )
                computeStatuses _ status =
                    ( Nothing, (status == Entering || status == Entered) ? Just Exiting $ Nothing)

            in
                def
                    { construct = do
                        (status,next) <- computeInitialStatuses
                        TS status def <$> newIORef def <*> newIORef next <*> newIORef def

                    , mounted = do
                        TS {..} <- get self
                        writeIORef mounted True
                        updateStatus

                    , receive = \newprops oldstate -> do
                        oldprops <- ask self
                        TS {..}  <- get self
                        let (current,upcoming) = computeStatuses (visible newprops) status
                        writeIORef next upcoming
                        let newStatus = fromMaybe status current
                        return TS
                            { status = newStatus
                            , ..
                            }

                    , updated = \_ _ ->
                        updateStatus

                    , unmounted = do
                        TS {..} <- get self
                        writeIORef mounted False

                    , render = \Transition_ {..} TS {..} ->
                          let
                              animationClasses cs =
                                  cs ++
                                    [ animating # "animating"
                                    , case status of
                                        Entering -> "in"
                                        Exiting  -> "out"
                                        Exited   -> "hidden"
                                        _        -> def
                                    , (status /= Exited) # "visible"
                                    , "transition"
                                    ]

                              animationStyles styles =
                                  let ad =
                                          case status of
                                              Entering -> ("animation-duration",ms(calculateTransitionDuration status duration))
                                              Exiting  -> ("animation-duration",ms(calculateTransitionDuration status duration))
                                              _        -> def
                                  in styles <> [ ad ]

                          in
                              (status /= Unmounted) #
                                    case animation of
                                        SomeTheme t -> 
                                          as ( Theme t features 
                                               & Classes (animationClasses []) 
                                               & Styles (animationStyles [])
                                             ) 
                                             children

                    }

instance HasProp As Transition where
    type Prop As Transition = Features -> [View] -> View
    getProp _ = as
    setProp _ a t = t { as = a }

instance HasChildren Transition where
    getChildren = children
    setChildren cs t = t { children = cs }

instance HasFeatures Transition where
    getFeatures = features
    setFeatures fs t = t { features = fs }

instance HasProp Animation Transition where
    type Prop Animation Transition = SomeTheme
    getProp _ Transition_ {..} = animation
    setProp _ a t = t { animation = a }

instance HasProp AnimationDuration Transition where
    type Prop AnimationDuration Transition = AnimationDuration
    getProp _ = duration
    setProp _ d t = t { duration = d }

instance HasProp Visible Transition where
    type Prop Visible Transition = Bool
    getProp _ = visible
    setProp _ v t = t { visible = v }

instance HasProp MountOnShow Transition where
    type Prop MountOnShow Transition = Bool
    getProp _ = mountOnShow
    setProp _ mos t = t { mountOnShow = mos }

instance HasProp OnComplete Transition where
    type Prop OnComplete Transition = TransitionStatus -> IO ()
    getProp _ = onComplete
    setProp _ oc t = t { onComplete = oc }

instance HasProp OnHide Transition where
    type Prop OnHide Transition = TransitionStatus -> IO ()
    getProp _ = onHide
    setProp _ oh t = t { onHide = oh }

instance HasProp OnShow Transition where
    type Prop OnShow Transition = TransitionStatus -> IO ()
    getProp _ = onShow
    setProp _ os t = t { onShow = os }

instance HasProp OnStart Transition where
    type Prop OnStart Transition = TransitionStatus -> IO ()
    getProp _ = onStart
    setProp _ os t = t { onStart = os }

instance HasProp TransitionOnMount Transition where
    type Prop TransitionOnMount Transition = Bool
    getProp _ = transitionOnMount
    setProp _ tom t = t { transitionOnMount = tom }

instance HasProp UnmountOnHide Transition where
    type Prop UnmountOnHide Transition = Bool
    getProp _ = unmountOnHide
    setProp _ uoh t = t { unmountOnHide = uoh }

data Group = Group_
    { as :: Features -> [(Int,View)] -> View
    , features :: Features
    , children :: [(Int,View)]
    , duration :: AnimationDuration
    , animation :: SomeTheme
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef :: Group)
        { as = \fs cs -> (Keyed Div) & Features fs & KeyedChildren cs
        , animation = fade
        , duration = Uniform 500
        }

pattern Group :: Group -> Group
pattern Group tg = tg

data GroupState = TGS
    { buffer :: [(Int,View)]
    }

instance Pure Group where
    view =
        LibraryComponentIO $ \self ->
            let
                handleOnHide key _ =
                    modify_ self $ \_ TGS {..} -> TGS { buffer = Prelude.filter ((/= key) . fst) buffer, .. }

                wrapChild anim dur vis tom (key,child) =
                    (key,View $ Transition def
                        { duration = dur
                        , animation = anim
                        , transitionOnMount = tom
                        , visible = vis
                        , onHide = handleOnHide key
                        , children = [ child ]
                        }
                    )

                hide :: View -> View
                hide (View Transition_ {..}) = View Transition_ { visible = False, .. }

                fromTransition (Just (View t@Transition_ {})) f = Just (f t)
                fromTransition _ _ = Nothing

            in def
                { construct = do
                    tg@Group_ {..} <- ask self
                    return TGS
                        { buffer = fmap (wrapChild animation duration True False) children
                        }

                , receive = \Group_ { duration = dur, children = cs, .. } TGS {..} -> return TGS
                    { buffer = flip fmap (mergeMappings buffer cs) $ \(k,c) ->
                        let prevChild = lookup k buffer
                            hasPrev   = isJust prevChild
                            hasNext   = isJust (lookup k cs)
                            leaving   = fromMaybe False (fromTransition prevChild (not . visible))
                            entering  = hasNext && (not hasPrev || leaving)
                            exiting   = not hasNext && hasPrev && not leaving

                        in if | entering  -> wrapChild animation dur True True (k,c)
                              | exiting   -> (k,hide (fromJust prevChild))
                              | otherwise -> fromJust $ fromTransition prevChild $ \Transition_ {..} ->
                                                 wrapChild animation dur visible transitionOnMount (k,c)

                    , ..
                    }

                , render = \Group_ {..} TGS {..} -> as features buffer
                }

instance HasProp As Group where
    type Prop As Group = Features -> [(Int,View)] -> View
    getProp _ = as
    setProp _ a tg = tg { as = a }

instance HasFeatures Group where
    getFeatures = features
    setFeatures as tg = tg { features = as }

instance HasKeyedChildren Group where
    getKeyedChildren = children
    setKeyedChildren cs tg = tg { children = cs }

instance HasProp Animation Group where
    type Prop Animation Group = SomeTheme
    getProp _ Group_ {..} = animation
    setProp _ a tg = tg { animation = a }

instance HasProp AnimationDuration Group where
    type Prop AnimationDuration Group = AnimationDuration
    getProp _ = duration
    setProp _ d tg = tg { duration = d }

data SomeTheme = forall t. Themeable t => SomeTheme t

data AnimationStyles = AnimationStyles
  { baseTransitionStyles :: CSS ()
  , onTransition :: CSS ()
  , onIn :: CSS ()
  , onOut :: CSS ()
  , animationStyles :: CSS ()
  }

renderAnimationStyles c AnimationStyles {..} = do
    is ".transition" . is c $ do
        baseTransitionStyles 
        onTransition
        is ".in" onIn
        is ".out" onOut
    animationStyles

instance Default SomeTheme where def = fade

instance Default AnimationStyles where
    def = AnimationStyles {..} 
      where
        baseTransitionStyles = void $ do
            apply $ do
                animIter one
                animDur 300
                "-webkit-animation-timing-function" =: ease
                "animation-timing-function"        =: ease
                "-webkit-animation-fill-mode"       =: both
                "animation-fill-mode"              =: both
            is ".animating" .> do
                "-webkit-backface-visibility" =: hidden
                "backface-visibility"        =: hidden
                important $ visibility        =: "visible"
            is ".loading" .> do
                position =: absolute
                top      =: pxs (-99999)
                left     =: pxs (-99999)
            is ".hidden" .> do
                visibility =: hidden
                display    =: none
            is ".visible" .> do
                important $ display    =: block
                important $ visibility =: "visible"
            is ".disabled" .> do
                "-webkit-animation-play-state" =: "paused"
                "animation-play-state"        =: "paused"
            is ".looping" .> animIter infinite
        animationStyles = return ()
        onTransition = return ()
        onIn = return ()
        onOut = return ()

data Browse = Browse
browse = SomeTheme Browse
instance Themeable Browse where
    theme c _ = renderAnimationStyles c def 
        { onTransition = void $ apply $ animDur 500
        , onIn = void $ apply $ animName "browseIn"
        , onOut = void $ do
            apply       $  animName "browseOutLeft"
            is ".left"  .> animName "browseOutLeft"
            is ".right" .> animName "browseOutRight"
        , animationStyles = void $ do
            keyframes "browseIn" $ do
                let f p s z mo = 
                        is (per p) .> do
                            trans $ scale(s) <<>> translateZ(pxs 0)
                            zIndex =: z
                            maybe (return ()) (\o -> void $ opacity =: o) mo
                f 0   (dec 0.8)  (neg one)  Nothing
                f 10  (dec 0.8)  (neg one) (Just "0.7")
                f 80  (dec 1.05) "999" (Just "1")
                f 100 (dec 1.05) "999" Nothing
            keyframes "browseOutLeft" $ do
                let f p z tx ry rx tz mo =
                        is (per p) .> do
                            zIndex =: z
                            trans $ translateX(tx) <<>> rotY ry <<>> rotX rx <<>> maybe "" translateZ tz
                            maybe (return ()) (\o -> void $ opacity =: o) mo
                f 0  (int 999) (per 0) (deg 0) (deg 0) Nothing Nothing
                f 50 (neg one) (neg (per 105)) (deg 35) (deg 10) (Just (neg (pxs 10))) Nothing
                is (per 80) .> opacity =: one
                f 100 (neg one) (per 0) (deg 0) (deg 0) (Just (neg (pxs 10))) (Just zero)
            keyframes "browseOutRight" $ do
                let f p z tx ry rx tz mo =
                        is (per p) .> do
                            zIndex =: z
                            trans $ translateX(tx) <<>> rotY ry <<>> rotX rx <<>> maybe "" translateZ tz
                            maybe (return ()) (\o -> void $ opacity =: o) mo
                f 0 "999" (per 0) (deg 0) (deg 0) Nothing Nothing
                f 50 "1" (per 105) (deg 35) (deg 35) (Just (neg (pxs 10))) Nothing
                is (per 80) .> opacity =: one
                f 100 "1" (per 0) (deg 0) (deg 0) (Just (neg (pxs 10))) (Just zero)
        }

data Drop = Drop
drop = SomeTheme Drop
instance Themeable Drop where
    theme c _ = renderAnimationStyles c def
      { onTransition = void $ apply $ do
        transOrigin (top <<>> center)
        animDur 400
        animTiming (0.34,1.61,0.7,1)
      , onIn = void $ apply $ animName "dropIn"
      , onOut = void $ apply $ animName "dropOut"
      , animationStyles = void $ do
        keyframes "dropIn" $ do
            is (per 0) .> do
                opacity =: zero
                trans $ scale zero
            is (per 100) .> do
                opacity =: one
                trans $ scale one
        keyframes "dropOut" $ do
            is (per 0)    .> do
                opacity =: one
                trans $ scale one
            is (per 100) .> do
                opacity =: zero
                trans $ scale zero
      }

data Fade = Fade
fade = SomeTheme Fade
instance Themeable Fade where
    theme c _ = renderAnimationStyles c def
      { onIn = void $ apply $ animName "fadeIn"
      , onOut = void $ apply $ animName "fadeOut"
      , animationStyles = void $ do
        keyframes "fadeIn" $ do
            is (per 0)   .> opacity =: zero
            is (per 100) .> opacity =: one
        keyframes "fadeOut" $ do
            is (per 0)   .> opacity =: one
            is (per 100) .> opacity =: zero
      }

data FadeUp = FadeUp
fadeUp = SomeTheme FadeUp
instance Themeable FadeUp where
    theme c _ = renderAnimationStyles c def
      { onIn = void $ apply $ animName "fadeInUp"
      , onOut = void $ apply $ animName "fadeOutUp"
      , animationStyles = void $ do
        keyframes "fadeInUp" $ do
            is (per 0) .> do
                opacity =: zero
                trans (translateY(per 10))
            is (per 100) .> do
                opacity =: one
                trans (translateY(per 0))
        keyframes "fadeOutUp" $ do
            is (per 0) .> do
                opacity =: one
                trans (translateY(per 0))
            is (per 100) .> do
                opacity =: zero
                trans (translateY(per 5))
      }

data FadeDown = FadeDown
fadeDown = SomeTheme FadeDown
instance Themeable FadeDown where
    theme c _ = renderAnimationStyles c def
      { onIn = void $ apply $ animName "fadeInDown"
      , onOut = void $ apply $ animName "fadeOutDown"
      , animationStyles = void $ do
        keyframes "fadeInDown" $ do
            is (per 0) .> do
                opacity =: zero
                trans (translateY(neg (per 10)))
            is (per 100) .> do
                opacity =: one
                trans (translateY(per 0))
        keyframes "fadeOutDown" $ do
            is (per 0) .> do
                opacity =: one
                trans (translateY(per 0))
            is (per 100) .> do
                opacity =: zero
                trans (translateY(neg (per 5)))
      }

data FadeLeft = FadeLeft
fadeLeft = SomeTheme FadeLeft
instance Themeable FadeLeft where
    theme c _ = renderAnimationStyles c def
      { onIn = void $ apply $ animName "fadeInLeft"
      , onOut = void $ apply $ animName "fadeOutLeft" 
      , animationStyles = void $ do
        keyframes "fadeInLeft" $ do
            is (per 0) .> do
                opacity =: zero
                trans (translateX(per 10))
            is (per 100) .> do
                opacity =: one
                trans (translateX(per 0))
        keyframes "fadeOutLeft" $ do
            is (per 0) .> do
                opacity =: one
                trans (translateX(per 0))
            is (per 100) .> do
                opacity =: zero
                trans (translateX(per 5))
      }

data FadeRight = FadeRight
fadeRight = SomeTheme FadeRight
instance Themeable FadeRight where
    theme c _ = renderAnimationStyles c def
      { onIn = void $ apply $ animName "fadeInRight"
      , onOut = void $ apply $ animName "fadeOutRight"
      , animationStyles = void $ do
        keyframes "fadeInRight" $ do
            is (per 0) .> do
                opacity =: zero
                trans (translateX(neg (per 10)))
            is (per 100) .> do
                opacity =: one
                trans (translateX(per 0))
        keyframes "fadeOutRight" $ do
            is (per 0) .> do
                opacity =: one
                trans (translateX(per 0))
            is (per 100) .> do
                opacity =: zero
                trans (translateX(neg (per 5)))
      }

data HorizontalFlip = HorizontalFlip
horizontalFlip = SomeTheme HorizontalFlip
instance Themeable HorizontalFlip where
    theme c _ = renderAnimationStyles c def
      { onIn = void $ apply $ animName "horizontalFlipIn"
      , onOut = void $ apply $ animName "horizontalFlipOut"
      , animationStyles = void $ do
        keyframes "horizontalFlipIn" $ do
            is (per 0) .> do
                opacity =: zero
                trans (persp (pxs 2000) <<>> rotY(neg (deg 90)))
            is (per 100) .> do
                opacity =: one
                trans (persp (pxs 2000) <<>> rotY(deg 0))
        keyframes "horizontalFlipOut" $ do
            is (per 0) .> do
                opacity =: one
                trans (persp (pxs 2000) <<>> rotY(deg 0))
            is (per 100) .> do
                opacity =: zero
                trans (persp (pxs 2000) <<>> rotY(deg 90))

      }