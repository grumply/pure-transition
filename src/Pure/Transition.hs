{-# LANGUAGE PatternSynonyms, ViewPatterns, TypeFamilies, KindSignatures, 
    OverloadedStrings, MultiWayIf, ExistentialQuantification, 
    DuplicateRecordFields, RecordWildCards, MultiParamTypeClasses, 
    DeriveGeneric, DeriveAnyClass, FlexibleContexts, TypeApplications, 
    ScopedTypeVariables #-}
module Pure.Transition where

-- from pure
import Pure hiding (Transition,Left,Right,ZoomIn,ZoomOut,animation,visible)

-- from pure-cond
import Pure.Data.Cond

-- from pure-theme
import Pure.Theme hiding (visible)
import qualified Pure.Theme as CSS

-- from pure-prop
import Pure.Data.Prop

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

import Debug.Trace
import Unsafe.Coerce

data As = As_
pattern As :: HasProp As a => Prop As a -> a -> a
pattern As p a <- (getProp As_ &&& id -> (p,a)) where
    As p a = setProp As_ p a

data InAnimation = InAnimation_
pattern InAnimation :: HasProp InAnimation a => Prop InAnimation a -> a -> a
pattern InAnimation p a <- (getProp InAnimation_ &&& id -> (p,a)) where
    InAnimation p a = setProp InAnimation_ p a

data OutAnimation = OutAnimation_
pattern OutAnimation :: HasProp OutAnimation a => Prop OutAnimation a -> a -> a
pattern OutAnimation p a <- (getProp OutAnimation_ &&& id -> (p,a)) where
    OutAnimation p a = setProp OutAnimation_ p a

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
    , inAnimation :: SomeInTheme
    , outAnimation :: SomeOutTheme
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
                        Transition_ {..} <- ask self
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
                                  case (inAnimation,outAnimation) of
                                      (SomeInTheme it,SomeOutTheme ot) ->
                                          as ( themed it
                                             $ themed ot
                                             $ Classes (animationClasses []) 
                                             $ Styles (animationStyles [])
                                             $ features
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

instance HasProp InAnimation Transition where
    type Prop InAnimation Transition = SomeInTheme
    getProp _ Transition_ {..} = inAnimation
    setProp _ a t = t { inAnimation = a }

instance HasProp OutAnimation Transition where
    type Prop OutAnimation Transition = SomeOutTheme
    getProp _ Transition_ {..} = outAnimation
    setProp _ a t = t { outAnimation = a }

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
    , inAnimation :: SomeInTheme
    , outAnimation :: SomeOutTheme
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef :: Group)
        { as = \fs cs -> (Keyed Div) & Features fs & KeyedChildren cs
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

                wrapChild inAnim outAnim dur vis tom (key,child) =
                    (key,View $ Transition def
                        { duration = dur
                        , inAnimation = inAnim
                        , outAnimation = outAnim
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
                        { buffer = fmap (wrapChild inAnimation outAnimation duration True False) children
                        }

                , receive = \Group_ { duration = dur, children = cs, .. } TGS {..} -> return TGS
                    { buffer = flip fmap (mergeMappings buffer cs) $ \(k,c) ->
                        let prevChild = lookup k buffer
                            hasPrev   = isJust prevChild
                            hasNext   = isJust (lookup k cs)
                            leaving   = fromMaybe False (fromTransition prevChild (not . visible))
                            entering  = hasNext && (not hasPrev || leaving)
                            exiting   = not hasNext && hasPrev && not leaving

                        in if | entering  -> wrapChild inAnimation outAnimation dur True True (k,c)
                              | exiting   -> (k,hide (fromJust prevChild))
                              | otherwise -> fromJust $ fromTransition prevChild $ \Transition_ {..} ->
                                                 wrapChild inAnimation outAnimation dur visible transitionOnMount (k,c)

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

instance HasProp InAnimation Group where
    type Prop InAnimation Group = SomeInTheme
    getProp _ Group_ {..} = inAnimation
    setProp _ a tg = tg { inAnimation = a }

instance HasProp OutAnimation Group where
    type Prop OutAnimation Group = SomeOutTheme
    getProp _ Group_ {..} = outAnimation
    setProp _ a tg = tg { outAnimation = a }

instance HasProp AnimationDuration Group where
    type Prop AnimationDuration Group = AnimationDuration
    getProp _ = duration
    setProp _ d tg = tg { duration = d }

-- Transitions from semantic-ui: https://github.com/Semantic-Org/Semantic-UI-CSS/blob/master/components/transition.css

data SomeInTheme = forall t. Theme t => SomeInTheme t
instance Default SomeInTheme where def = Pure.Transition.fadeIn

data SomeOutTheme = forall t. Theme t => SomeOutTheme t
instance Default SomeOutTheme where def = fadeOut

data AnimationStyles 
    = InAnimationStyles
        { animationStyles :: CSS ()
        , onTransition :: CSS ()
        , onAnimation :: CSS ()
        }
    | OutAnimationStyles
        { animationStyles :: CSS ()
        , onTransition :: CSS ()
        , onAnimation :: CSS ()
        }
    | StaticAnimationStyles
        { animationStyles :: CSS ()
        , onTransition :: CSS ()
        , onAnimation :: CSS ()
        }
    deriving (Generic,Default)

data NoAnimation = NoAnimation 
noInAnimation = SomeInTheme NoAnimation
noOutAnimation = SomeOutTheme NoAnimation
instance Theme NoAnimation where
    theme _ = return ()

simpleInAnimation name frames = InAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ animName name
    , onAnimation = void $ keyframes name frames
    }

simpleOutAnimation name frames = OutAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ animName name
    , onAnimation = void $ keyframes name frames
    }

simpleStaticAnimation name dur frames = StaticAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = do
        void $ apply $ do
            animDur dur
            animName name 
    , onAnimation = void $ keyframes name frames
    }

renderAnimationStyles c InAnimationStyles {..} = do
    is ".transition" . is c $ do
        animationStyles 
        is ".in" onTransition
    onAnimation
renderAnimationStyles c OutAnimationStyles {..} = do
    is ".transition" . is c $ do
        animationStyles 
        is ".out" onTransition
    onAnimation
renderAnimationStyles c StaticAnimationStyles {..} = do
    is ".transition" . is c $ do
        animationStyles
        onTransition
    onAnimation

defaultAnimationStyles = void $ do
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

data BrowseIn = BrowseIn
browseIn = SomeInTheme BrowseIn
browseInAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles 
    , onTransition = void $ apply $ do
        animDur 500
        animName "browseIn" 
    , onAnimation = void $ keyframes "browseIn" $ do
        let f p s z mo = 
                is (per p) .> do
                    trans $ Pure.Data.Styles.scale(s) <<>> translateZ(pxs 0)
                    zIndex =: z
                    maybe (return ()) (\o -> void $ opacity =: o) mo
        f 0   (dec 0.8)  (neg one)  Nothing
        f 10  (dec 0.8)  (neg one) (Just "0.7")
        f 80  (dec 1.05) "999" (Just "1")
        f 100 (dec 1.05) "999" Nothing
    }
instance Theme BrowseIn where
    theme c = renderAnimationStyles c browseInAnimation

data BrowseOutLeft = BrowseOutLeft
browseOutLeft = SomeOutTheme BrowseOutLeft
browseOutLeftAnimation = OutAnimationStyles 
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animDur 500
        animName "browseOutLeft"
    , onAnimation = void $ keyframes "browseOutLeft" $ do
        let f p z tx ry rx tz mo =
                is (per p) .> do
                    zIndex =: z
                    trans $ translateX(tx) <<>> rotY ry <<>> rotX rx <<>> maybe "" translateZ tz
                    maybe (return ()) (\o -> void $ opacity =: o) mo
        f 0  (int 999) (per 0) (deg 0) (deg 0) Nothing Nothing
        f 50 (neg one) (neg (per 105)) (deg 35) (deg 10) (Just (neg (pxs 10))) Nothing
        is (per 80) .> opacity =: one
        f 100 (neg one) (per 0) (deg 0) (deg 0) (Just (neg (pxs 10))) (Just zero)
    }
instance Theme BrowseOutLeft where
    theme c = renderAnimationStyles c browseOutLeftAnimation

data BrowseOutRight = BrowseOutRight
browseOutRight = SomeOutTheme BrowseOutRight
browseOutRightAnimation = OutAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animDur 500
        animName "browseOutRight" 
    , onAnimation = void $ keyframes "browseOutRight" $ do
        let f p z tx ry rx tz mo =
                is (per p) .> do
                    zIndex =: z
                    trans $ translateX(tx) <<>> rotY ry <<>> rotX rx <<>> maybe "" translateZ tz
                    maybe (return ()) (\o -> void $ opacity =: o) mo
        f 0 (int 999) (per 0) (deg 0) (deg 0) Nothing Nothing
        f 50 one (per 105) (deg 35) (deg 35) (Just (neg (pxs 10))) Nothing
        is (per 80) .> opacity =: one
        f 100 one (per 0) (deg 0) (deg 0) (Just (neg (pxs 10))) (Just zero)
    }
instance Theme BrowseOutRight where
    theme c = renderAnimationStyles c browseOutRightAnimation

data DropIn = DropIn
dropIn = SomeInTheme DropIn
dropInAnimation = InAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do 
          transOrigin (top <<>> center)
          animDur 400
          animTiming (0.34,1.61,0.7,1)
          animName "dropIn"
    , onAnimation = void $ keyframes "dropIn" $ do
          is (per 0) .> do
              opacity =: zero
              trans $ Pure.Data.Styles.scale zero
          is (per 100) .> do
              opacity =: one
              trans $ Pure.Data.Styles.scale one
    }
instance Theme DropIn where
    theme c = renderAnimationStyles c dropInAnimation

data DropOut = DropOut
dropOut = SomeOutTheme DropOut
dropOutAnimation = OutAnimationStyles
    { animationStyles = defaultAnimationStyles 
    , onTransition = void $ apply $ do
          transOrigin (top <<>> center)
          animDur 400
          animTiming (0.34,1.61,0.7,1)
          animName "dropOut"
    , onAnimation = void $ keyframes "dropOut" $ do
          is (per 0)    .> do
              opacity =: one
              trans $ Pure.Data.Styles.scale one
          is (per 100) .> do
              opacity =: zero
              trans $ Pure.Data.Styles.scale zero
    }
instance Theme DropOut where
    theme c = renderAnimationStyles c dropOutAnimation

data FadeIn = FadeIn
fadeIn = SomeInTheme FadeIn
fadeInAnimation = simpleInAnimation "fadeIn" $ do
    is (per 0)   .> opacity =: zero
    is (per 100) .> opacity =: one
instance Theme FadeIn where
    theme c = renderAnimationStyles c fadeInAnimation

data FadeOut = FadeOut
fadeOut = SomeOutTheme FadeOut
fadeOutAnimation = simpleOutAnimation "fadeOut" $ do
    is (per 0)   .> opacity =: one
    is (per 100) .> opacity =: zero
instance Theme FadeOut where
    theme c = renderAnimationStyles c fadeOutAnimation

data FadeInUp = FadeInUp
fadeInUp = SomeInTheme FadeInUp
fadeInUpAnimation = simpleInAnimation "fadeInUp" $ do
    is (per 0) .> do
        opacity =: zero
        trans (translateY(per 10))
    is (per 100) .> do
        opacity =: one
        trans (translateY(per 0))
instance Theme FadeInUp where
    theme c = renderAnimationStyles c fadeInUpAnimation

data FadeOutUp = FadeOutUp
fadeOutUp = SomeOutTheme FadeOutUp
fadeOutUpAnimation = simpleOutAnimation "fadeOutUp" $ do
    is (per 0) .> do
        opacity =: one
        trans (translateY(per 0))
    is (per 100) .> do
        opacity =: zero
        trans (translateY(per 5))
instance Theme FadeOutUp where
    theme c = renderAnimationStyles c fadeOutUpAnimation

data FadeInDown = FadeInDown
fadeInDown = SomeInTheme FadeInDown
fadeInDownAnimation = simpleInAnimation "fadeInDown" $ do
    is (per 0) .> do
        opacity =: zero
        trans (translateY(neg (per 10)))
    is (per 100) .> do
        opacity =: one
        trans (translateY(per 0))
instance Theme FadeInDown where
    theme c = renderAnimationStyles c fadeInDownAnimation

data FadeOutDown = FadeOutDown
fadeOutDown = SomeOutTheme FadeOutDown
fadeOutDownAnimation = simpleOutAnimation "fadeOutDown" $ do
    is (per 0) .> do
        opacity =: one
        trans (translateY(per 0))
    is (per 100) .> do
        opacity =: zero
        trans (translateY(neg (per 5)))
instance Theme FadeOutDown where
    theme c = renderAnimationStyles c fadeOutDownAnimation

data FadeInLeft = FadeInLeft
fadeInLeft = SomeInTheme FadeInLeft
fadeInLeftAnimation = simpleInAnimation "fadeInLeft" $ do
    is (per 0) .> do
        opacity =: zero
        trans (translateX(per 10))
    is (per 100) .> do
        opacity =: one
        trans (translateX(per 0))
instance Theme FadeInLeft where
    theme c = renderAnimationStyles c fadeInLeftAnimation

data FadeOutLeft = FadeOutLeft
fadeOutLeft = SomeOutTheme FadeOutLeft
fadeOutLeftAnimation = simpleOutAnimation "fadeOutLeft" $ do
    is (per 0) .> do
        opacity =: one
        trans (translateX(per 0))
    is (per 100) .> do
        opacity =: zero
        trans (translateX(per 5))
instance Theme FadeOutLeft where
    theme c = renderAnimationStyles c fadeOutLeftAnimation

data FadeInRight = FadeInRight
fadeInRight = SomeInTheme FadeInRight
fadeInRightAnimation = simpleInAnimation "fadeInRight" $ do
    is (per 0) .> do
        opacity =: zero
        trans (translateX(neg (per 10)))
    is (per 100) .> do
        opacity =: one
        trans (translateX(per 0))
instance Theme FadeInRight where
    theme c = renderAnimationStyles c fadeInRightAnimation

data FadeOutRight = FadeOutRight
fadeOutRight = SomeOutTheme FadeOutRight
fadeOutRightAnimation = simpleOutAnimation "fadeOutRight" $ do
    is (per 0) .> do
        opacity =: one
        trans (translateX(per 0))
    is (per 100) .> do
        opacity =: zero
        trans (translateX(neg (per 5)))
instance Theme FadeOutRight where
    theme c = renderAnimationStyles c fadeOutRightAnimation

data HorizontalFlipIn = HorizontalFlipIn
horizontalFlipIn = SomeInTheme HorizontalFlipIn
horizontalFlipInAnimation = simpleInAnimation "horizontalFlipIn" $ do
    is (per 0) .> do
        opacity =: zero
        trans (persp (pxs 2000) <<>> rotY(neg (deg 90)))
    is (per 100) .> do
        opacity =: one
        trans (persp (pxs 2000) <<>> rotY(deg 0))
instance Theme HorizontalFlipIn where
    theme c = renderAnimationStyles c horizontalFlipInAnimation

data HorizontalFlipOut = HorizontalFlipOut
horizontalFlipOut = SomeOutTheme HorizontalFlipOut
horizontalFlipOutAnimation = simpleOutAnimation "horizontalFlipOut" $ do
    is (per 0) .> do
        opacity =: one
        trans (persp (pxs 2000) <<>> rotY(deg 0))
    is (per 100) .> do
        opacity =: zero
        trans (persp (pxs 2000) <<>> rotY(deg 90))
instance Theme HorizontalFlipOut where
    theme c = renderAnimationStyles c horizontalFlipOutAnimation

data VerticalFlipIn = VerticalFlipIn
verticalFlipIn = SomeInTheme VerticalFlipIn
verticalFlipInAnimation = simpleInAnimation "verticalFlipIn" $ do
    is (per 0) .> do
        opacity =: zero
        trans (persp (pxs 2000) <<>> rotX(neg (deg 90)))
    is (per 100) .> do
        opacity =: one
        trans (persp (pxs 2000) <<>> rotX(deg 0))
instance Theme VerticalFlipIn where
    theme c = renderAnimationStyles c verticalFlipInAnimation

data VerticalFlipOut = VerticalFlipOut
verticalFlipOut = SomeOutTheme VerticalFlipOut
verticalFlipOutAnimation = simpleOutAnimation "verticalFlipOut" $ do
    is (per 0) .> do
        opacity =: one
        trans (persp (pxs 2000) <<>> rotX(deg 0))
    is (per 100) .> do
        opacity =: zero
        trans (persp (pxs 2000) <<>> rotX(deg 90))
instance Theme VerticalFlipOut where
    theme c = renderAnimationStyles c verticalFlipOutAnimation

data ScaleIn = ScaleIn
scaleIn = SomeInTheme ScaleIn
scaleInAnimation = simpleInAnimation "scaleIn" $ do
    is (per 0) .> do
        opacity =: zero
        trans $ Pure.Data.Styles.scale (dec 0.8)
    is (per 100) .> do
        opacity =: one
        trans $ Pure.Data.Styles.scale (int 1)
instance Theme ScaleIn where
    theme c = renderAnimationStyles c scaleInAnimation

data ScaleOut = ScaleOut
scaleOut = SomeOutTheme ScaleOut
scaleOutAnimation = simpleOutAnimation "scaleOut" $ do
    is (per 0) .> do
        opacity =: one
        trans $ Pure.Data.Styles.scale (int 1)
    is (per 100) .> do
        opacity =: zero
        trans $ Pure.Data.Styles.scale (dec 0.9)
instance Theme ScaleOut where
    theme c = renderAnimationStyles c scaleOutAnimation

simpleFlyInAnimation name animation = InAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animDur 600
        transTiming (0.215,0.16,0.355,1)
        animName name
    , onAnimation = void $ keyframes name animation
    }

simpleFlyOutAnimation name animation = OutAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animDur 600
        transTiming (0.215,0.16,0.355,1)
        animName name
    , onAnimation = void $ keyframes name animation
    }

data FlyIn = FlyIn
flyIn = SomeInTheme FlyIn
flyInAnimation = simpleFlyInAnimation "flyIn" $ do
    let f p mo s3d = 
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ scale3 s3d
    f 0 (Just zero) 0.3
    f 20 Nothing 1.1
    f 40 Nothing 0.9
    f 60 (Just one) 1.03
    f 80 Nothing 0.97
    f 100 (Just one) 1
instance Theme FlyIn where
    theme c = renderAnimationStyles c flyInAnimation

data FlyOut = FlyOut
flyOut = SomeOutTheme FlyOut
flyOutAnimation = simpleFlyOutAnimation "flyOut" $ do
    let f p mo s3d = 
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ scale3 s3d
    f 20 Nothing 0.9
    f 50 (Just one) 1.1
    f 55 (Just one) 1.1
    f 100 (Just zero) 0.3
instance Theme FlyOut where
    theme c = renderAnimationStyles c flyOutAnimation

data FlyInUp = FlyInUp
flyInUp = SomeInTheme FlyInUp
flyInUpAnimation = simpleFlyInAnimation "flyInUp" $ do
    let f p mo t3d = 
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ translate3d zero t3d zero
    f 0 (Just zero) (pxs 1500)
    f 60 (Just one) (neg (pxs 20))
    f 75 Nothing (pxs 10)
    f 90 Nothing (neg (pxs 5))
    f 100 Nothing zero
instance Theme FlyInUp where
    theme c = renderAnimationStyles c flyInUpAnimation

data FlyOutUp = FlyOutUp
flyOutUp = SomeOutTheme FlyOutUp
flyOutUpAnimation = simpleFlyOutAnimation "flyOutUp" $ do
    let f p mo t3d = 
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ translate3d zero t3d zero
    f 20 Nothing (pxs 10)
    f 40 (Just one) (neg (pxs 20))
    f 45 (Just one) (neg (pxs 20))
instance Theme FlyOutUp where
    theme c = renderAnimationStyles c flyOutUpAnimation

data FlyInDown = FlyInDown
flyInDown = SomeInTheme FlyInDown
flyInDownAnimation = simpleFlyInAnimation "flyInDown" $ do
    let f p mo t3d =
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ translate3d zero t3d zero
    f 0 (Just zero) (neg (pxs 1500))
    f 60 (Just one) (pxs 25)
    f 75 Nothing (neg (pxs 10))
    f 90 Nothing (neg (pxs 5))
    is (per 100) .> trans "none"
instance Theme FlyInDown where
    theme c = renderAnimationStyles c flyInDownAnimation

data FlyOutDown = FlyOutDown
flyOutDown = SomeOutTheme FlyOutDown
flyOutDownAnimation = simpleFlyOutAnimation "flyOutDown" $ do
    let f p mo t3d =
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ translate3d zero t3d zero
    f 20 Nothing (neg (pxs 10))
    f 40 (Just one) (pxs 20)
    f 45 (Just one) (pxs 20)
    f 100 (Just zero) (neg (pxs 2000))
instance Theme FlyOutDown where
    theme c = renderAnimationStyles c flyOutDownAnimation

data FlyInLeft = FlyInLeft
flyInLeft = SomeInTheme FlyInLeft
flyInLeftAnimation = simpleFlyInAnimation "flyInLeft" $ do
    let f p mo t3d =
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ translate3d t3d zero zero
    f 0 (Just zero) (pxs 1500)
    f 60 (Just one) (neg (pxs 25))
    f 75 Nothing (pxs 10)
    f 90 Nothing (neg (pxs 5))
    is (per 100) .> trans "none"
instance Theme FlyInLeft where
    theme c = renderAnimationStyles c flyInLeftAnimation

data FlyOutLeft = FlyOutLeft
flyOutLeft = SomeOutTheme FlyOutLeft
flyOutLeftAnimation = simpleFlyOutAnimation "flyOutLeft" $ do
    let f p mo t3d =
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ translate3d t3d zero zero
    f 20 (Just one) (neg (pxs 20))
    f 100 (Just zero) (pxs 2000)
instance Theme FlyOutLeft where
    theme c = renderAnimationStyles c flyOutLeftAnimation

data FlyInRight = FlyInRight
flyInRight = SomeInTheme FlyInRight
flyInRightAnimation = simpleFlyInAnimation "flyInRight" $ do
    let f p mo t3d =
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ translate3d t3d zero zero
    f 0 (Just zero) (neg (pxs 1500))
    f 60 (Just one) (pxs 25)
    f 75 Nothing (neg (pxs 10))
    f 90 Nothing (pxs 5)
    is (per 100) .> trans "none"
instance Theme FlyInRight where
    theme c = renderAnimationStyles c flyInRightAnimation

data FlyOutRight = FlyOutRight
flyOutRight = SomeOutTheme FlyOutRight
flyOutRightAnimation = simpleFlyOutAnimation "flyOutRight" $ do
    let f p mo t3d =
            is (per p) .> do
                maybe (return ()) (void . (opacity =:)) mo
                trans $ translate3d t3d zero zero
    f 20 (Just one) (pxs 20)
    f 100 (Just zero) (neg (pxs 2000))
instance Theme FlyOutRight where
    theme c = renderAnimationStyles c flyOutRightAnimation

data SlideInDown = SlideInDown
slideInDown = SomeInTheme SlideInDown
slideInDownAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do 
          transOrigin (top <<>> center)
          animName "slideInDown"
    , onAnimation = void $ keyframes "slideInDown" $ do
          is (per 0) .> do
              opacity =: zero
              trans $ scaleY zero
          is (per 100) .> do
              opacity =: one
              trans $ scaleY one
    }
instance Theme SlideInDown where
    theme c = renderAnimationStyles c slideInDownAnimation

data SlideInUp = SlideInUp
slideInUp = SomeInTheme SlideInUp
slideInUpAnimation = InAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do 
          transOrigin (bottom <<>> center)
          animName "slideInUp"
    , onAnimation = void $ keyframes "slideInUp" $ do
          is (per 0) .> do
              opacity =: zero
              trans $ scaleY zero
          is (per 100) .> do
              opacity =: one
              trans $ scaleY one
    }
instance Theme SlideInUp where
    theme c = renderAnimationStyles c slideInUpAnimation

data SlideInLeft = SlideInLeft
slideInLeft = SomeInTheme SlideInLeft
slideInLeftAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
            transOrigin (center <<>> right)
            animName "slideInLeft"
    , onAnimation = void $ keyframes "slideInLeft" $ do
          is (per 0) .> do
              opacity =: zero
              trans $ scaleX zero
          is (per 100) .> do
              opacity =: one
              trans $ scaleX one
    }
instance Theme SlideInLeft where
    theme c = renderAnimationStyles c slideInLeftAnimation

data SlideInRight = SlideInRight
slideInRight = SomeInTheme SlideInRight
slideInRightAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
            transOrigin (center <<>> left)
            animName "slideInRight"
    , onAnimation = void $ keyframes "slideInRight" $ do
          is (per 0) .> do
              opacity =: zero
              trans $ scaleX zero
          is (per 100) .> do
              opacity =: one
              trans $ scaleX one
    }
instance Theme SlideInRight where
    theme c = renderAnimationStyles c slideInRightAnimation

data SlideOutDown = SlideOutDown
slideOutDown = SomeInTheme SlideOutDown
slideOutDownAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do 
          transOrigin (top <<>> center)
          animName "slideOutDown"
    , onAnimation = void $ keyframes "slideOutDown" $ do
          is (per 0) .> do
              opacity =: one
              trans $ scaleY one
          is (per 100) .> do
              opacity =: zero
              trans $ scaleY zero
    }
instance Theme SlideOutDown where
    theme c = renderAnimationStyles c slideOutDownAnimation

data SlideOutUp = SlideOutUp
slideOutUp = SomeInTheme SlideOutUp
slideOutUpAnimation = InAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do 
          transOrigin (bottom <<>> center)
          animName "slideOutUp"
    , onAnimation = void $ keyframes "slideOutUp" $ do
          is (per 0) .> do
              opacity =: one
              trans $ scaleY one
          is (per 100) .> do
              opacity =: zero
              trans $ scaleY zero
    }
instance Theme SlideOutUp where
    theme c = renderAnimationStyles c slideOutUpAnimation

data SlideOutLeft = SlideOutLeft
slideOutLeft = SomeInTheme SlideOutLeft
slideOutLeftAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
            transOrigin (center <<>> right)
            animName "slideOutLeft"
    , onAnimation = void $ keyframes "slideOutLeft" $ do
          is (per 0) .> do
              opacity =: one
              trans $ scaleX one
          is (per 100) .> do
              opacity =: zero
              trans $ scaleX zero
    }
instance Theme SlideOutLeft where
    theme c = renderAnimationStyles c slideOutLeftAnimation

data SlideOutRight = SlideOutRight
slideOutRight = SomeInTheme SlideOutRight
slideOutRightAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
            transOrigin (center <<>> left)
            animName "slideOutRight"
    , onAnimation = void $ keyframes "slideOutRight" $ do
          is (per 0) .> do
              opacity =: one
              trans $ scaleX one
          is (per 100) .> do
              opacity =: zero
              trans $ scaleX zero
    }
instance Theme SlideOutRight where
    theme c = renderAnimationStyles c slideOutRightAnimation

swingInAnimation name orig frames = InAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animDur 600
        transOrigin orig
        animName name
    , onAnimation = void $ keyframes name frames
    }

swingOutAnimation name orig frames = OutAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animDur 600
        transOrigin orig
        animName name
    , onAnimation = void $ keyframes name frames
    }

swingFrames ((p,exy,mo):fs) = do
    is (per p) .> do
        trans $ persp(pxs 1000) <<>> either rotX rotY exy
        maybe (return ()) (void . (opacity =:)) mo
    swingFrames fs
swingFrames [] = return ()

data SwingInDown = SwingInDown
swingInDown = SomeInTheme SwingInDown
swingInDownAnimation = swingInAnimation "swingInDown" (top <<>> center) $ swingFrames
    [ (0,Left $ deg 90,Just zero)
    , (40,Left $ neg (deg 30),Just one)
    , (60,Left $ deg 15,Nothing)
    , (80,Left $ neg (deg 7.5),Nothing)
    , (100,Left $ deg 0,Nothing)
    ]
instance Theme SwingInDown where
    theme c = renderAnimationStyles c swingInDownAnimation

data SwingInUp = SwingInUp
swingInUp = SomeInTheme SwingInUp
swingInUpAnimation = swingInAnimation "swingInUp" (bottom <<>> center) $ swingFrames
    [ (0,Left $ deg 90,Just zero)
    , (40,Left $ neg (deg 30),Just one)
    , (60,Left $ deg 15,Nothing)
    , (80,Left $ neg (deg 7.5),Nothing)
    , (100,Left $ deg 0,Nothing)
    ]
instance Theme SwingInUp where
    theme c = renderAnimationStyles c swingInUpAnimation

data SwingInLeft = SwingInLeft
swingInLeft = SomeInTheme SwingInLeft
swingInLeftAnimation = swingInAnimation "swingInLeft" (center <<>> right) $ swingFrames
    [ (0,Right $ neg (deg 90),Just zero)
    , (40,Right $ deg 30,Just one)
    , (60,Right $ neg (deg 17.5),Nothing)
    , (80,Right $ deg 7.5,Nothing)
    , (100,Right $ deg 0,Nothing)
    ]
instance Theme SwingInLeft where
    theme c = renderAnimationStyles c swingInLeftAnimation

data SwingInRight = SwingInRight
swingInRight = SomeInTheme SwingInRight
swingInRightAnimation = swingInAnimation "swingInRight" (center <<>> left) $ swingFrames
    [ (0,Right $ neg (deg 90),Just zero)
    , (40,Right $ deg 30,Just one)
    , (60,Right $ neg (deg 17.5),Nothing)
    , (80,Right $ deg 7.5,Nothing)
    , (100,Right $ deg 0,Nothing)
    ]
instance Theme SwingInRight where
    theme c = renderAnimationStyles c swingInRightAnimation

data SwingOutDown = SwingOutDown
swingOutDown = SomeOutTheme SwingOutDown
swingOutDownAnimation = swingOutAnimation "swingOutDown" (top <<>> center) $ swingFrames
    [ (0,Left $ deg 0,Nothing)
    , (40,Left $ neg (deg 7.5),Nothing)
    , (60,Left $ deg 17.5,Nothing)
    , (80,Left $ neg (deg 30),Just one)
    , (100,Left $ deg 90,Just zero)
    ]
instance Theme SwingOutDown where
    theme c = renderAnimationStyles c swingOutDownAnimation

data SwingOutUp = SwingOutUp
swingOutUp = SomeOutTheme SwingOutUp
swingOutUpAnimation = swingOutAnimation "swingOutUp" (bottom <<>> center) $ swingFrames
    [ (0,Left $ deg 0,Nothing)
    , (40,Left $ neg (deg 7.5),Nothing)
    , (60,Left $ deg 17.5,Nothing)
    , (80,Left $ neg (deg 30),Just one)
    , (100,Left $ deg 90,Just zero)
    ]
instance Theme SwingOutUp where
    theme c = renderAnimationStyles c swingOutUpAnimation

data SwingOutLeft = SwingOutLeft
swingOutLeft = SomeOutTheme SwingOutLeft
swingOutLeftAnimation = swingOutAnimation "swingOutLeft" (center <<>> right) $ swingFrames
    [ (0,Right $ deg 0,Nothing)
    , (40,Right $ deg 7.5,Nothing)
    , (60,Right $ neg (deg 10),Nothing)
    , (80,Right $ deg 30,Just one)
    , (100,Right $ neg (deg 90),Just zero)
    ]
instance Theme SwingOutLeft where
    theme c = renderAnimationStyles c swingOutLeftAnimation

data SwingOutRight = SwingOutRight
swingOutRight = SomeOutTheme SwingOutRight
swingOutRightAnimation = swingOutAnimation "swingOutRight" (center <<>> left) $ swingFrames
    [ (0,Right $ deg 0,Nothing)
    , (40,Right $ deg 7.5,Nothing)
    , (60,Right $ neg (deg 10),Nothing)
    , (80,Right $ deg 30,Just one)
    , (100,Right $ neg (deg 90),Just zero)
    ]
instance Theme SwingOutRight where
    theme c = renderAnimationStyles c swingOutRightAnimation

data ZoomIn = ZoomIn
zoomIn = SomeInTheme ZoomIn
zoomInAnimation = simpleInAnimation "zoomIn" $ do
    is (per 0) .> do
        opacity =: one
        trans $ scale zero
    is (per 100) .> do
        opacity =: one
        trans $ scale one
instance Theme ZoomIn where
    theme c = renderAnimationStyles c zoomInAnimation

data ZoomOut = ZoomOut
zoomOut = SomeOutTheme ZoomOut
zoomOutAnimation = simpleOutAnimation "zoomOut" $ do
    is (per 0) .> do
        opacity =: one
        trans $ scale one
    is (per 100) .> do
        opacity =: one
        trans $ scale zero
instance Theme ZoomOut where
    theme c = renderAnimationStyles c zoomOutAnimation

data Flash = Flash
flash = SomeInTheme Flash
flashAnimation = simpleStaticAnimation "flash" 750 $ do
    is (per 0 <&>> per 50 <&>> per 100) .> opacity =: one
    is (per 25 <&>> per 75) .> opacity =: zero
instance Theme Flash where
    theme c = renderAnimationStyles c flashAnimation

data Shake = Shake
shake = SomeInTheme Shake
shakeAnimation = simpleStaticAnimation "shake" 750 $ do
    is (per 0 <&>> per 100) .> 
      trans (translateX zero)

    is (per 10 <&>> per 30 <&>> per 50 <&>> per 70 <&>> per 90) .> 
      trans (translateX (neg (pxs 10)))

    is (per 20 <&>> per 40 <&>> per 60 <&>> per 80) .> 
      trans (translateX (pxs 10))
instance Theme Shake where
    theme c = renderAnimationStyles c shakeAnimation

data Bounce = Bounce
bounce = SomeInTheme Bounce
bounceAnimation = simpleStaticAnimation "bounce" 750 $ do
    is (per 0 <&>> per 20 <&>> per 50 <&>> per 80 <&>> per 100) .>
      trans (translateY zero)
    is (per 40) .>
      trans (translateY (neg (pxs 30)))
    is (per 60) .>
      trans (translateY (neg (pxs 15)))
instance Theme Bounce where
    theme c = renderAnimationStyles c bounceAnimation

data Tada = Tada
tada = SomeInTheme Tada
tadaAnimation = simpleStaticAnimation "tada" 750 $ do
    is (per 0) .>
      trans (scale one)
    is (per 10 <&>> per 20) .>
      trans (scale(dec 0.9) <<>> rotate(neg (deg 3)))
    is (per 30 <&>> per 50 <&>> per 70 <&>> per 90) .>
      trans (scale(dec 1.1) <<>> rotate(deg 3))
    is (per 40 <&>> per 60 <&>> per 80) .>
      trans (scale(dec 1.1) <<>> rotate(neg (deg 3)))
    is (per 100) .>
      trans (scale one <<>> rotate zero)
instance Theme Tada where
    theme c = renderAnimationStyles "tada" tadaAnimation

data Pulse = Pulse
pulse = SomeInTheme Pulse
pulseAnimation = simpleStaticAnimation "pulse" 750 $ do
    is (per 0) .> do
      trans (scale one)
      opacity =: one
    is (per 50) .> do
      trans (scale (dec 0.9))
      opacity =: dec 0.7
    is (per 100) .> do
        trans (scale one)
        opacity =: one
instance Theme Pulse where
    theme c = renderAnimationStyles c pulseAnimation

data Jiggle = Jiggle
jiggle = SomeInTheme Jiggle
jiggleAnimation = simpleStaticAnimation "jiggle" 750 $ do
    let fs = [ (0,1,1,1), (30,1.25,0.75,1), (40,0.75,1.25,1), (50,1.15,0.85,1), (65,0.95,1.05,1), (75,1.05,0.95,1), (100,1,1,1) ]
    for_ fs $ \(p,x,y,z) -> is (per p) .> trans ("scale3d(" <> dec x <&>> dec y <&>> dec z <> ")")
instance Theme Jiggle where
    theme c = renderAnimationStyles c jiggleAnimation

data Glow = Glow
glow = SomeInTheme Glow
glowAnimation = StaticAnimationStyles 
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
          animDur 2000 
          animName "glow"
          animTiming (0.19,1,0.22,1)
    , onAnimation = void $ keyframes "glow" $ do
        is (per 0) .> 
          backgroundColor =: "#FCFCFD"
        is (per 30) .>
          backgroundColor =: "#FFF6CD"
        is (per 100) .>
          backgroundColor =: "#FCFCFD"
    }
instance Theme Glow where
    theme c = renderAnimationStyles c glowAnimation