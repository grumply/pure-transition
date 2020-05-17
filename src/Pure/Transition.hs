{-# LANGUAGE PatternSynonyms, ViewPatterns, TypeFamilies, KindSignatures, 
    OverloadedStrings, MultiWayIf, ExistentialQuantification, 
    DuplicateRecordFields, RecordWildCards, MultiParamTypeClasses, 
    DeriveGeneric, DeriveAnyClass, FlexibleContexts, TypeApplications, 
    ScopedTypeVariables, PostfixOperators #-}
module Pure.Transition where

-- from pure
import Pure hiding (Transition,Left,Right,ZoomIn,ZoomOut,not,(#))

-- from pure-cond
import Pure.Data.Cond

-- from pure-theme
import Pure.Theme hiding (not,(#))
import qualified Pure.Theme as CSS

-- from pure-prop
import Pure.Data.Prop

-- from pure-styles
import Pure.Data.Styles hiding (not,(#))

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

import Prelude hiding (or)

-- A port of pure-semantic-ui's Transition with themeing

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
    , duration_ :: AnimationDuration
    , visible_ :: Bool
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
        , duration_ = Uniform 500
        , visible_ = True
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
        Component $ \self ->
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
                                threadDelay (calculateTransitionDuration s duration_ * 1000)
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
                        if | visible_ && transitionOnMount -> (Exited   ,Just Entering)
                           | visible_                      -> (Entered  ,Nothing)
                           | mountOnShow || unmountOnHide  -> (Unmounted,Nothing)
                           | otherwise                     -> (Exited   ,Nothing)

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
                        let (current,upcoming) = computeStatuses (visible_ newprops) status
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
                              animationClasses =
                                [ animating # "animating"
                                , case status of
                                    Entering -> "in"
                                    Exiting  -> out
                                    Exited   -> hidden
                                    _        -> def
                                , (status /= Exited) # visible
                                , transition
                                ]

                              animationStyles =
                                [ case status of
                                    Entering -> (animation-duration,(calculateTransitionDuration status duration_) <#> ms)
                                    Exiting  -> (animation-duration,(calculateTransitionDuration status duration_) <#> ms)
                                    _        -> def
                                ]

                          in
                              (status /= Unmounted) #
                                  case (inAnimation,outAnimation) of
                                      (SomeInTheme it,SomeOutTheme ot) ->
                                          as ( themed it
                                             $ themed ot
                                             $ Classes animationClasses
                                             $ Styles animationStyles
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
    getProp _ = duration_
    setProp _ d t = t { duration_ = d }

instance HasProp Visible Transition where
    type Prop Visible Transition = Bool
    getProp _ = visible_
    setProp _ v t = t { visible_ = v }

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
    , duration_ :: AnimationDuration
    , inAnimation :: SomeInTheme
    , outAnimation :: SomeOutTheme
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef :: Group)
        { as = \fs cs -> (Keyed Div) & Features fs & KeyedChildren cs
        , duration_ = Uniform 500
        }

pattern Group :: Group -> Group
pattern Group tg = tg

data GroupState = TGS
    { buffer :: [(Int,View)]
    }

instance Pure Group where
    view =
        Component $ \self ->
            let
                handleOnHide key _ =
                    modify_ self $ \_ TGS {..} -> TGS { buffer = Prelude.filter ((/= key) . fst) buffer, .. }

                wrapChild inAnim outAnim dur vis tom (key,child) =
                    (key,View $ Transition def
                        { duration_ = dur
                        , inAnimation = inAnim
                        , outAnimation = outAnim
                        , transitionOnMount = tom
                        , visible_ = vis
                        , onHide = handleOnHide key
                        , children = [ child ]
                        }
                    )

                hide :: View -> View
                hide (View Transition_ {..}) = View Transition_ { visible_ = False, .. }

                fromTransition (Just (View t@Transition_ {})) f = Just (f t)
                fromTransition _ _ = Nothing

            in def
                { construct = do
                    tg@Group_ {..} <- ask self
                    return TGS
                        { buffer = fmap (wrapChild inAnimation outAnimation duration_ True False) children
                        }

                , receive = \Group_ { duration_ = dur, children = cs, .. } TGS {..} -> return TGS
                    { buffer = flip fmap (mergeMappings buffer cs) $ \(k,c) ->
                        let prevChild = lookup k buffer
                            hasPrev   = isJust prevChild
                            hasNext   = isJust (lookup k cs)
                            leaving   = fromMaybe False (fromTransition prevChild (not . visible_))
                            entering  = hasNext && (not hasPrev || leaving)
                            exiting   = not hasNext && hasPrev && not leaving

                        in if | entering  -> wrapChild inAnimation outAnimation dur True True (k,c)
                              | exiting   -> (k,hide (fromJust prevChild))
                              | otherwise -> fromJust $ fromTransition prevChild $ \Transition_ {..} ->
                                                 wrapChild inAnimation outAnimation dur visible_ transitionOnMount (k,c)

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
    getProp _ = duration_
    setProp _ d tg = tg { duration_ = d }

-- Transitions from semantic-ui: https://github.com/Semantic-Org/Semantic-UI-CSS/blob/master/comp1nts/transition.css

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
    , onTransition = 
        void $ apply $ 
            animation-name =: name
    , onAnimation = 
        void $ 
            atKeyframes name frames
    }

simpleOutAnimation name frames = OutAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition =
        void $ apply $ 
            animation-name =: name
    , onAnimation = 
        void $ 
            atKeyframes name frames
    }

simpleStaticAnimation name dur frames = StaticAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = do
        void $ apply $ do
            animation-duration =: dur
            animation-name =: name 
    , onAnimation = 
        void $ 
            atKeyframes name frames
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
        animation-iteration-count        =: 1
        animation-duration               =: 300
        webkit-animation-timing-function =: ease
        animation-timing-function        =: ease
        webkit-animation-fill-mode       =: both
        animation-fill-mode              =: both

    is ".animating" .> do
        backface-visibility        =: hidden
        important $ visibility     =: visible

    is ".loading" .> do
        position =: absolute
        top      =: (-99999)px
        left     =: (-99999)px

    is ".hidden" .> do
        visibility =: hidden
        display    =: none

    is ".visible" .> do
        important $ display    =: block
        important $ visibility =: visible

    is ".disabled" .> do
        animation-play-state =: paused

    is ".looping" .> 
        animation-iteration-count =: infinite

data BrowseIn = BrowseIn
browseIn = SomeInTheme BrowseIn
browseInAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles 

    , onTransition = void $ apply $ do
        animation-duration =: 500
        animation-name =: "browseIn" 

    , onAnimation = void $ atKeyframes "browseIn" $ do
        -- translate3d forces GPU-acceleration in these transforms
        is (0%) .> do
            transform =: scale(0.8) <<>> translate3d(0,0,0)
            z-index   =: (-1)

        is (10%) .> do
            transform =: scale(0.8) <<>> translate3d(0,0,0)
            z-index   =: (-1)

        is (80%) .> do
            transform =: scale(1.05) <<>> translate3d(0,0,0)
            z-index   =: 999
            opacity   =: 0.7

        is (100%) .> do
            transform =: scale(1.05) <<>> translate3d(0,0,0)
            z-index   =: 999
            opacity   =: 1
    }
instance Theme BrowseIn where
    theme c = renderAnimationStyles c browseInAnimation

data BrowseOutLeft = BrowseOutLeft
browseOutLeft = SomeOutTheme BrowseOutLeft
browseOutLeftAnimation = OutAnimationStyles 
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do
        animation-duration =: 500
        animation-name =: "browseOutLeft"

    , onAnimation = void $ atKeyframes "browseOutLeft" $ do
        is (0%) .> do
            z-index   =: 999
            transform =: translateX(0%) <<>> rotateY(0deg) <<>> rotateX(0deg)

        is (50%) .> do
            z-index   =: (-1)
            transform =: translateX((-105)%) <<>> rotateY(35deg) <<>> rotateX(10deg) <<>> translateZ((-10)px)

        is (80%) .> do
            opacity =: 1

        is (100%) .> do
            z-index   =: (-1)
            transform =: translateX(0%) <<>> rotateY(0deg) <<>> rotateX(0deg) <<>> translateZ((-10)px)
            opacity   =: 0
    }
instance Theme BrowseOutLeft where
    theme c = renderAnimationStyles c browseOutLeftAnimation

data BrowseOutRight = BrowseOutRight
browseOutRight = SomeOutTheme BrowseOutRight
browseOutRightAnimation = OutAnimationStyles
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do
        animation-duration =: 500
        animation-name =: "browseOutRight" 

    , onAnimation = void $ atKeyframes "browseOutRight" $ do
        is (0%) .> do
            z-index   =: 999
            transform =: translateX(0%) <<>> rotateY(0deg) <<>> rotateX(0deg)

        is (50%) .> do
            z-index   =: 1
            transform =: translateX((-105)%) <<>> rotateY(35deg) <<>> rotateX(10deg) <<>> translateZ((-10)px)

        is (80%) .> do
            opacity =: 1

        is (100%) .> do
            z-index   =: 1
            transform =: translateX(0%) <<>> rotateY(0deg) <<>> rotateX(0deg) <<>> translateZ((-10)px)
            opacity   =: 0
    }
instance Theme BrowseOutRight where
    theme c = renderAnimationStyles c browseOutRightAnimation

data DropIn = DropIn
dropIn = SomeInTheme DropIn
dropInAnimation = InAnimationStyles
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do 
          transform-origin          =* [top,center]
          animation-duration        =: 400
          animation-timing-function =: cubez(0.34,1.61,0.7,1)
          animation-name            =: "dropIn"

    , onAnimation = void $ atKeyframes "dropIn" $ do
          is (0%) .> do
              opacity   =: 0
              transform =: scale(0)

          is (100%) .> do
              opacity   =: 1
              transform =: scale(1)
    }
instance Theme DropIn where
    theme c = renderAnimationStyles c dropInAnimation

data DropOut = DropOut
dropOut = SomeOutTheme DropOut
dropOutAnimation = OutAnimationStyles
    { animationStyles = defaultAnimationStyles 

    , onTransition = void $ apply $ do
          transform-origin          =* [top,center]
          animation-duration        =: 400
          animation-timing-function =: cubez(0.34,1.61,0.7,1)
          animation-name            =: "dropOut"

    , onAnimation = void $ atKeyframes "dropOut" $ do
          is (0%) .> do
              opacity   =: 1
              transform =: scale(1)

          is (100%) .> do
              opacity   =: 0
              transform =: scale(0)
    }
instance Theme DropOut where
    theme c = renderAnimationStyles c dropOutAnimation

data FadeIn = FadeIn
fadeIn = SomeInTheme FadeIn
fadeInAnimation = simpleInAnimation "fadeIn" $ do
    is (0%) .> 
        opacity =: 0
    is (100%) .> 
        opacity =: 1
instance Theme FadeIn where
    theme c = renderAnimationStyles c fadeInAnimation

data FadeOut = FadeOut
fadeOut = SomeOutTheme FadeOut
fadeOutAnimation = simpleOutAnimation "fadeOut" $ do
    is (0%) .> 
        opacity =: 1
    is (100%) .> 
        opacity =: 0
instance Theme FadeOut where
    theme c = renderAnimationStyles c fadeOutAnimation

data FadeInUp = FadeInUp
fadeInUp = SomeInTheme FadeInUp
fadeInUpAnimation = simpleInAnimation "fadeInUp" $ do
    is (0%) .> do
        opacity =: 0
        transform =: translateY(10%)
    is (100%) .> do
        opacity =: 1
        transform =: translateY(0%)
instance Theme FadeInUp where
    theme c = renderAnimationStyles c fadeInUpAnimation

data FadeOutUp = FadeOutUp
fadeOutUp = SomeOutTheme FadeOutUp
fadeOutUpAnimation = simpleOutAnimation "fadeOutUp" $ do
    is (0%) .> do
        opacity =: 1
        transform =: translateY(0%)
    is (100%) .> do
        opacity =: 0
        transform =: translateY(5%)
instance Theme FadeOutUp where
    theme c = renderAnimationStyles c fadeOutUpAnimation

data FadeInDown = FadeInDown
fadeInDown = SomeInTheme FadeInDown
fadeInDownAnimation = simpleInAnimation "fadeInDown" $ do
    is (0%) .> do
        opacity =: 0
        transform =: translateY((-10)%)
    is (100%) .> do
        opacity =: 1
        transform =: translateY(0%)
instance Theme FadeInDown where
    theme c = renderAnimationStyles c fadeInDownAnimation

data FadeOutDown = FadeOutDown
fadeOutDown = SomeOutTheme FadeOutDown
fadeOutDownAnimation = simpleOutAnimation "fadeOutDown" $ do
    is (0%) .> do
        opacity =: 1
        transform =: translateY(0%)
    is (100%) .> do
        opacity =: 0
        transform =: translateY((-5)%)
instance Theme FadeOutDown where
    theme c = renderAnimationStyles c fadeOutDownAnimation

data FadeInLeft = FadeInLeft
fadeInLeft = SomeInTheme FadeInLeft
fadeInLeftAnimation = simpleInAnimation "fadeInLeft" $ do
    is (0%) .> do
        opacity =: 0
        transform =: translateX(10%)
    is (100%) .> do
        opacity =: 1
        transform =: translateX(0%)
instance Theme FadeInLeft where
    theme c = renderAnimationStyles c fadeInLeftAnimation

data FadeOutLeft = FadeOutLeft
fadeOutLeft = SomeOutTheme FadeOutLeft
fadeOutLeftAnimation = simpleOutAnimation "fadeOutLeft" $ do
    is (0%) .> do
        opacity =: 1
        transform =: translateX(0%)
    is (100%) .> do
        opacity =: 0
        transform =: translateX(5%)
instance Theme FadeOutLeft where
    theme c = renderAnimationStyles c fadeOutLeftAnimation

data FadeInRight = FadeInRight
fadeInRight = SomeInTheme FadeInRight
fadeInRightAnimation = simpleInAnimation "fadeInRight" $ do
    is (0%) .> do
        opacity =: 0
        transform =: translateX((-10)%)
    is (100%) .> do
        opacity =: 1
        transform =: translateX(0%)
instance Theme FadeInRight where
    theme c = renderAnimationStyles c fadeInRightAnimation

data FadeOutRight = FadeOutRight
fadeOutRight = SomeOutTheme FadeOutRight
fadeOutRightAnimation = simpleOutAnimation "fadeOutRight" $ do
    is (0%) .> do
        opacity =: 1
        transform =: translateX(0%)
    is (100%) .> do
        opacity =: 0
        transform =: translateX((-5)%)
instance Theme FadeOutRight where
    theme c = renderAnimationStyles c fadeOutRightAnimation

data HorizontalFlipIn = HorizontalFlipIn
horizontalFlipIn = SomeInTheme HorizontalFlipIn
horizontalFlipInAnimation = simpleInAnimation "horizontalFlipIn" $ do
    is (0%) .> do
        opacity =: 0
        transform =: persp(2000px) <<>> rotY((-90)deg)
    is (100%) .> do
        opacity =: 1
        transform =: persp(2000px) <<>> rotY(0deg)
instance Theme HorizontalFlipIn where
    theme c = renderAnimationStyles c horizontalFlipInAnimation

data HorizontalFlipOut = HorizontalFlipOut
horizontalFlipOut = SomeOutTheme HorizontalFlipOut
horizontalFlipOutAnimation = simpleOutAnimation "horizontalFlipOut" $ do
    is (0%) .> do
        opacity =: 1
        transform =: persp(2000px) <<>> rotY(0deg)
    is (100%) .> do
        opacity =: 0
        transform =: persp(2000px) <<>> rotY(90deg)
instance Theme HorizontalFlipOut where
    theme c = renderAnimationStyles c horizontalFlipOutAnimation

data VerticalFlipIn = VerticalFlipIn
verticalFlipIn = SomeInTheme VerticalFlipIn
verticalFlipInAnimation = simpleInAnimation "verticalFlipIn" $ do
    is (0%) .> do
        opacity =: 0
        transform =: persp(2000px) <<>> rotX(-(90)deg)
    is (100%) .> do
        opacity =: 1
        transform =: persp(2000px) <<>> rotX(0deg)
instance Theme VerticalFlipIn where
    theme c = renderAnimationStyles c verticalFlipInAnimation

data VerticalFlipOut = VerticalFlipOut
verticalFlipOut = SomeOutTheme VerticalFlipOut
verticalFlipOutAnimation = simpleOutAnimation "verticalFlipOut" $ do
    is (0%) .> do
        opacity =: 1
        transform =: persp(2000px) <<>> rotX(0deg)
    is (100%) .> do
        opacity =: 0
        transform =: persp(2000px) <<>> rotX(90deg)
instance Theme VerticalFlipOut where
    theme c = renderAnimationStyles c verticalFlipOutAnimation

data ScaleIn = ScaleIn
scaleIn = SomeInTheme ScaleIn
scaleInAnimation = simpleInAnimation "scaleIn" $ do
    is (0%) .> do
        opacity =: 0
        transform =: scale(0.8)
    is (100%) .> do
        opacity =: 1
        transform =: scale(1)
instance Theme ScaleIn where
    theme c = renderAnimationStyles c scaleInAnimation

data ScaleOut = ScaleOut
scaleOut = SomeOutTheme ScaleOut
scaleOutAnimation = simpleOutAnimation "scaleOut" $ do
    is (0%) .> do
        opacity =: 1
        transform =: scale(1)
    is (100%) .> do
        opacity =: 0
        transform =: scale(0.9)
instance Theme ScaleOut where
    theme c = renderAnimationStyles c scaleOutAnimation

simpleFlyInAnimation name anim = InAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animation-duration         =: 600
        transition-timing-function =: cubez(0.215,0.16,0.355,1)
        animation-name             =: name
    , onAnimation = void $ atKeyframes name anim
    }

simpleFlyOutAnimation name anim = OutAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animation-duration         =: 600
        transition-timing-function =: cubez(0.215,0.16,0.355,1)
        animation-name             =: name
    , onAnimation = void $ atKeyframes name anim
    }

data FlyIn = FlyIn
flyIn = SomeInTheme FlyIn
flyInAnimation = simpleFlyInAnimation "flyIn" $ do
    is (0%) .> do
        opacity =: 0
        transform =: scale3d(0.3,0.3,0.3)

    is (20%) .>
        transform =: scale3d(1.1,1.1,1.1)

    is (40%) .>
        transform =: scale3d(0.9,0.9,0.9)

    is (60%) .> do 
        opacity =: 1
        transform =: scale3d(1.03,1.03,1.03)

    is (80%) .>
        transform =: scale3d(0.97,0.97,0.97)

    is (100%) .> do
        opacity =: 1
        transform =: scale3d(1,1,1)

instance Theme FlyIn where
    theme c = renderAnimationStyles c flyInAnimation

data FlyOut = FlyOut
flyOut = SomeOutTheme FlyOut
flyOutAnimation = simpleFlyOutAnimation "flyOut" $ do
    is (20%) .> do
        transform =: scale3d(0.9,0.9,0.9)

    is (50%) . or is (55%) .> do
        opacity =: 1
        transform =: scale3d(1.1,1.1,1.1)

    is (100%) .> do
        opacity =: 0
        transform =: scale3d(0.3,0.3,0.3)

instance Theme FlyOut where
    theme c = renderAnimationStyles c flyOutAnimation

data FlyInUp = FlyInUp
flyInUp = SomeInTheme FlyInUp
flyInUpAnimation = simpleFlyInAnimation "flyInUp" $ do
    is (0%) .> do
        opacity =: 0
        transform =: translate3d(0,1500px,0)

    is (60%) .> do
        opacity =: 1
        transform =: translate3d(0,(-20)px,0)

    is (75%) .>
        transform =: translate3d(0,10px,0)

    is (90%) .>
        transform =: translate3d(0,(-5)px,0)

    is (100%) .>
        transform =: translate3d(0,0,0)


instance Theme FlyInUp where
    theme c = renderAnimationStyles c flyInUpAnimation

data FlyOutUp = FlyOutUp
flyOutUp = SomeOutTheme FlyOutUp
flyOutUpAnimation = simpleFlyOutAnimation "flyOutUp" $ do
    is (20%) .>
        transform =: translate3d(0,10px,0)

    is (40%) . or is (45%) .> do
        opacity   =: 1
        transform =: translate3d(0,(-20)px,0);

    is (100%) .> do
        opacity   =: 0
        transform =: translate3d(0,2000px,0);

instance Theme FlyOutUp where
    theme c = renderAnimationStyles c flyOutUpAnimation

data FlyInDown = FlyInDown
flyInDown = SomeInTheme FlyInDown
flyInDownAnimation = simpleFlyInAnimation "flyInDown" $ do
    is (0%) .> do
        opacity =: 0
        transform =: translate3d(0,(-1500)px,0)

    is (60%) .> do
        opacity =: 1
        transform =: translate3d(0,25px,0)

    is (75%) .>
        transform =: translate3d(0,(-10)px,0)

    is (90%) .>
        transform =: translate3d(0,5px,0)

    is (100%) .>
        transform =: none

instance Theme FlyInDown where
    theme c = renderAnimationStyles c flyInDownAnimation

data FlyOutDown = FlyOutDown
flyOutDown = SomeOutTheme FlyOutDown
flyOutDownAnimation = simpleFlyOutAnimation "flyOutDown" $ do
    is (20%) .>
        transform =: translate3d(0,(-10)px,0)

    is (40%) . is (45%) .> do
        opacity =: 1
        transform =: translate3d(0,20px,0)

    is (100%) .> do
        opacity =: 0
        transform =: translate3d(0,(-2000)px,0)

instance Theme FlyOutDown where
    theme c = renderAnimationStyles c flyOutDownAnimation

data FlyInLeft = FlyInLeft
flyInLeft = SomeInTheme FlyInLeft
flyInLeftAnimation = simpleFlyInAnimation "flyInLeft" $ do
    is (0%) .> do
        opacity =: 0
        transform =: translate3d(1500px,0,0)

    is (60%) .> do
        opacity =: 1
        transform =: translate3d((-25)px,0,0)

    is (75%) .>
        transform =: translate3d(10px,0,0)

    is (90%) .>
        transform =: translate3d((-5)px,0,0)

    is (100%) .>
        transform =: none

instance Theme FlyInLeft where
    theme c = renderAnimationStyles c flyInLeftAnimation

data FlyOutLeft = FlyOutLeft
flyOutLeft = SomeOutTheme FlyOutLeft
flyOutLeftAnimation = simpleFlyOutAnimation "flyOutLeft" $ do
    is (20%) .> do
        opacity =: 1
        transform =: translate3d((-20)px,0,0)

    is (100%) .> do
        opacity =: 0
        transform =: translate3d(2000px,0,0)

instance Theme FlyOutLeft where
    theme c = renderAnimationStyles c flyOutLeftAnimation

data FlyInRight = FlyInRight
flyInRight = SomeInTheme FlyInRight
flyInRightAnimation = simpleFlyInAnimation "flyInRight" $ do
    is (0%) .> do
        opacity =: 0
        transform =: translate3d((-1500)px,0,0)
    
    is (60%) .> do
        opacity =: 1
        transform =: translate3d(25px,0,0)
    
    is (75%) .>
        transform =: translate3d((-10)px,0,0)

    is (90%) .>
        transform =: translate3d(5px,0,0)

    is (100%) .>
        transform =: none

instance Theme FlyInRight where
    theme c = renderAnimationStyles c flyInRightAnimation

data FlyOutRight = FlyOutRight
flyOutRight = SomeOutTheme FlyOutRight
flyOutRightAnimation = simpleFlyOutAnimation "flyOutRight" $ do
    is (20%) .> do
        opacity =: 1
        transform =: translate3d(20px,0,0)

    is (100%) .> do
        opacity =: 0
        transform =: translate3d((-2000)px,0,0)

instance Theme FlyOutRight where
    theme c = renderAnimationStyles c flyOutRightAnimation

data SlideInDown = SlideInDown
slideInDown = SomeInTheme SlideInDown
slideInDownAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do 
          transform-origin =* [top,center]
          animation-name =: "slideInDown"

    , onAnimation = void $ atKeyframes "slideInDown" $ do
          is (0%) .> do
              opacity   =: 0
              transform =: scaleY(0)

          is (100%) .> do
              opacity   =: 1
              transform =: scaleY(1)
    }
instance Theme SlideInDown where
    theme c = renderAnimationStyles c slideInDownAnimation

data SlideInUp = SlideInUp
slideInUp = SomeInTheme SlideInUp
slideInUpAnimation = InAnimationStyles
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do 
          transform-origin =* [bottom,center]
          animation-name =: "slideInUp"

    , onAnimation = void $ atKeyframes "slideInUp" $ do
          is (0%) .> do
              opacity   =: 0
              transform =: scaleY(0)

          is (100%) .> do
              opacity   =: 1
              transform =: scaleY(1)
    }
instance Theme SlideInUp where
    theme c = renderAnimationStyles c slideInUpAnimation

data SlideInLeft = SlideInLeft
slideInLeft = SomeInTheme SlideInLeft
slideInLeftAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do
        transform-origin =* [center,right]
        animation-name =: "slideInLeft"

    , onAnimation = void $ atKeyframes "slideInLeft" $ do
        is (0%) .> do
            opacity   =: 0
            transform =: scaleX(0)

        is (100%) .> do
            opacity   =: 1
            transform =: scaleX(1)
    }
instance Theme SlideInLeft where
    theme c = renderAnimationStyles c slideInLeftAnimation

data SlideInRight = SlideInRight
slideInRight = SomeInTheme SlideInRight
slideInRightAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do
        transform-origin =* [center,left]
        animation-name =: "slideInRight"

    , onAnimation = void $ atKeyframes "slideInRight" $ do
        is (0%) .> do
            opacity   =: 0
            transform =: scaleX(0)

        is (100%) .> do
            opacity   =: 1
            transform =: scaleX(1)
    }
instance Theme SlideInRight where
    theme c = renderAnimationStyles c slideInRightAnimation

data SlideOutDown = SlideOutDown
slideOutDown = SomeInTheme SlideOutDown
slideOutDownAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do 
        transform-origin =: (top <<>> center)
        animation-name =: "slideOutDown"

    , onAnimation = void $ atKeyframes "slideOutDown" $ do
        is (0%) .> do
            opacity =: 1
            transform =: scaleY(1)

        is (100%) .> do
            opacity =: 0
            transform =: scaleY(0)
    }
instance Theme SlideOutDown where
    theme c = renderAnimationStyles c slideOutDownAnimation

data SlideOutUp = SlideOutUp
slideOutUp = SomeInTheme SlideOutUp
slideOutUpAnimation = InAnimationStyles
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do 
        transform-origin =: (bottom <<>> center)
        animation-name =: "slideOutUp"

    , onAnimation = void $ atKeyframes "slideOutUp" $ do
        is (0%) .> do
            opacity   =: 1
            transform =: scaleY(1)

        is (100%) .> do
            opacity   =: 0
            transform =: scaleY(0)
    }
instance Theme SlideOutUp where
    theme c = renderAnimationStyles c slideOutUpAnimation

data SlideOutLeft = SlideOutLeft
slideOutLeft = SomeInTheme SlideOutLeft
slideOutLeftAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
            transform-origin =* [center,right]
            animation-name   =: "slideOutLeft"
    , onAnimation = void $ atKeyframes "slideOutLeft" $ do
          is (0%) .> do
              opacity   =: 1
              transform =: scaleX(1)

          is (100%) .> do
              opacity =: 0
              transform =: scaleX(0)
    }
instance Theme SlideOutLeft where
    theme c = renderAnimationStyles c slideOutLeftAnimation

data SlideOutRight = SlideOutRight
slideOutRight = SomeInTheme SlideOutRight
slideOutRightAnimation = InAnimationStyles 
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do
        transform-origin =* [center,left]
        animation-name   =: "slideOutRight"

    , onAnimation = void $ atKeyframes "slideOutRight" $ do
        is (0%) .> do
            opacity   =: 1
            transform =: scaleX(1)

        is (100%) .> do
            opacity   =: 0
            transform =: scaleX(0)
    }
instance Theme SlideOutRight where
    theme c = renderAnimationStyles c slideOutRightAnimation

swingInAnimation name orig frames = InAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animation-duration =: 600
        transform-origin   =: orig
        animation-name     =: name
    , onAnimation = void $ atKeyframes name frames
    }

swingOutAnimation name orig frames = OutAnimationStyles
    { animationStyles = defaultAnimationStyles
    , onTransition = void $ apply $ do
        animation-duration =: 600
        transform-origin   =: orig
        animation-name     =: name
    , onAnimation = void $ atKeyframes name frames
    }

swingFrames ((p,exy,mo):fs) = do
    is (percent p) .> do
        transform =: persp(1000px) <<>> either rotX rotY exy
        maybe (return ()) (void . (opacity =:)) mo
    swingFrames fs
swingFrames [] = return ()

data SwingInDown = SwingInDown
swingInDown = SomeInTheme SwingInDown
swingInDownAnimation = swingInAnimation "swingInDown" (top <<>> center) $ do
    is (0%) .> do
        opacity  =: 0
        transform =: persp(1000px) <<>> rotateX(90deg)

    is (40%) .> do
        opacity =: 1
        transform =: persp(1000px) <<>> rotateX((-30)deg)

    is (60%) .>
        transform =: persp(1000px) <<>> rotateX(15deg)

    is (80%) .>
        transform =: persp(1000px) <<>> rotateX((-7.5)deg)

    is (100%) .>
        transform =: persp(1000px) <<>> rotateX(0deg)

instance Theme SwingInDown where
    theme c = renderAnimationStyles c swingInDownAnimation

data SwingInUp = SwingInUp
swingInUp = SomeInTheme SwingInUp
swingInUpAnimation = swingInAnimation "swingInUp" (bottom <<>> center) $ do
    is (0%) .> do
        opacity  =: 0
        transform =: persp(1000px) <<>> rotateX((-90)deg)

    is (40%) .> do
        opacity =: 1
        transform =: persp(1000px) <<>> rotateX(30deg)

    is (60%) .>
        transform =: persp(1000px) <<>> rotateX((-15)deg)

    is (80%) .>
        transform =: persp(1000px) <<>> rotateX((7.5)deg)

    is (100%) .>
        transform =: persp(1000px) <<>> rotateX(0deg)

instance Theme SwingInUp where
    theme c = renderAnimationStyles c swingInUpAnimation

data SwingInLeft = SwingInLeft
swingInLeft = SomeInTheme SwingInLeft
swingInLeftAnimation = swingInAnimation "swingInLeft" (center <<>> right) $ do
    is (0%) .> do
        opacity  =: 0
        transform =: persp(1000px) <<>> rotateY((-90)deg)

    is (40%) .> do
        opacity =: 1
        transform =: persp(1000px) <<>> rotateY(30deg)

    is (60%) .>
        transform =: persp(1000px) <<>> rotateY((-15)deg)

    is (80%) .>
        transform =: persp(1000px) <<>> rotateY((7.5)deg)

    is (100%) .>
        transform =: persp(1000px) <<>> rotateY(0deg)

instance Theme SwingInLeft where
    theme c = renderAnimationStyles c swingInLeftAnimation

data SwingInRight = SwingInRight
swingInRight = SomeInTheme SwingInRight
swingInRightAnimation = swingInAnimation "swingInRight" (center <<>> left) $ do
    is (0%) .> do
        opacity  =: 0
        transform =: persp(1000px) <<>> rotateY((-90)deg)

    is (40%) .> do
        opacity =: 1
        transform =: persp(1000px) <<>> rotateY(30deg)

    is (60%) .>
        transform =: persp(1000px) <<>> rotateY((-15)deg)

    is (80%) .>
        transform =: persp(1000px) <<>> rotateY(7.5deg)

    is (100%) .>
        transform =: persp(1000px) <<>> rotateY(0deg)


instance Theme SwingInRight where
    theme c = renderAnimationStyles c swingInRightAnimation

data SwingOutDown = SwingOutDown
swingOutDown = SomeOutTheme SwingOutDown
swingOutDownAnimation = swingOutAnimation "swingOutDown" (top <<>> center) $ do
    is (0%) .>
        transform =: persp(1000px) <<>> rotateX(0deg)
    
    is (40%) .>
        transform =: persp(1000px) <<>> rotateX(7.5deg)

    is (60%) .>
        transform =: persp(1000px) <<>> rotateX(15deg)

    is (80%) .> do
        opacity   =: 1
        transform =: persp(1000px) <<>> rotateX((-30)deg)

    is (100%) .> do
        opacity   =: 0
        transform =: persp(1000px) <<>> rotateX(90deg)

instance Theme SwingOutDown where
    theme c = renderAnimationStyles c swingOutDownAnimation

data SwingOutUp = SwingOutUp
swingOutUp = SomeOutTheme SwingOutUp
swingOutUpAnimation = swingOutAnimation "swingOutUp" (bottom <<>> center) $ do
    is (0%) .>
        transform =: persp(1000px) <<>> rotateX(0deg)
    
    is (40%) .>
        transform =: persp(1000px) <<>> rotateX((-7.5)deg)

    is (60%) .>
        transform =: persp(1000px) <<>> rotateX(15deg)

    is (80%) .> do
        opacity   =: 1
        transform =: persp(1000px) <<>> rotateX((-30)deg)

    is (100%) .> do
        opacity   =: 0
        transform =: persp(1000px) <<>> rotateX(90deg)

instance Theme SwingOutUp where
    theme c = renderAnimationStyles c swingOutUpAnimation

data SwingOutLeft = SwingOutLeft
swingOutLeft = SomeOutTheme SwingOutLeft
swingOutLeftAnimation = swingOutAnimation "swingOutLeft" (center <<>> right) $ do
    is (0%) .>
        transform =: persp(1000px) <<>> rotateY(0deg)
    is (40%) .>
        transform =: persp(1000px) <<>> rotateY(7.5deg)
    is (60%) .>
        transform =: persp(1000px) <<>> rotateY((-15)deg)
    is (80%) .> do
        opacity   =: 1
        transform =: persp(1000px) <<>> rotateY(30deg)
    is (100%) .> do
        opacity   =: 0
        transform =: persp(1000px) <<>> rotateY((-90)deg)

instance Theme SwingOutLeft where
    theme c = renderAnimationStyles c swingOutLeftAnimation

data SwingOutRight = SwingOutRight
swingOutRight = SomeOutTheme SwingOutRight
swingOutRightAnimation = swingOutAnimation "swingOutRight" (center <<>> left) $ do
    is (0%) .>
        transform =: persp(1000px) <<>> rotateY(0deg)
    is (40%) .>
        transform =: persp(1000px) <<>> rotateY(7.5deg)
    is (60%) .>
        transform =: persp(1000px) <<>> rotateY((-15)deg)
    is (80%) .> do
        opacity   =: 1
        transform =: persp(1000px) <<>> rotateY(30deg)
    is (100%) .> do
        opacity   =: 0
        transform =: persp(1000px) <<>> rotateY((-90)deg)

instance Theme SwingOutRight where
    theme c = renderAnimationStyles c swingOutRightAnimation

data ZoomIn = ZoomIn
zoomIn = SomeInTheme ZoomIn
zoomInAnimation = simpleInAnimation "zoomIn" $ do
    is (0%) .> do
        opacity   =: 1
        transform =: scale(0)

    is (100%) .> do
        opacity   =: 1
        transform =: scale(1)

instance Theme ZoomIn where
    theme c = renderAnimationStyles c zoomInAnimation

data ZoomOut = ZoomOut
zoomOut = SomeOutTheme ZoomOut
zoomOutAnimation = simpleOutAnimation "zoomOut" $ do
    is (0%) .> do
        opacity   =: 1
        transform =: scale(1)

    is (100%) .> do
        opacity =: 1
        transform =: scale(0)

instance Theme ZoomOut where
    theme c = renderAnimationStyles c zoomOutAnimation

data Flash = Flash
flash = SomeInTheme Flash
flashAnimation = simpleStaticAnimation "flash" 750 $ do
    is (0%) . or is (50%) . or is (100%) .> 
        opacity =: 1

    is (25%) . or is (75%) .> 
        opacity =: 0

instance Theme Flash where
    theme c = renderAnimationStyles c flashAnimation

data Shake = Shake
shake = SomeInTheme Shake
shakeAnimation = simpleStaticAnimation "shake" 750 $ do
    is (0%) . or is (100%) .> 
      transform =: translateX(0)

    is (10%) . or is (30%) . or is (50%) . or is (70%) . or is (90%) .> 
      transform =: translateX((-10)px)

    is (20%) . or is (40%) . or is (60%) . or is (80%) .> 
      transform =: translateX(10px)

instance Theme Shake where
    theme c = renderAnimationStyles c shakeAnimation

data Bounce = Bounce
bounce = SomeInTheme Bounce
bounceAnimation = simpleStaticAnimation "bounce" 750 $ do
    is (0%) . or is (20%) . or is (50%) . or is (80%) . or is (100%) .>
      transform =: translateY(0)

    is (40%) .>
      transform =: translateY((-30)px)

    is (60%) .>
      transform =: translateY(15px)

instance Theme Bounce where
    theme c = renderAnimationStyles c bounceAnimation

data Tada = Tada
tada = SomeInTheme Tada
tadaAnimation = simpleStaticAnimation "tada" 750 $ do
    is (0%) .>
      transform =: scale(1)

    is (10%) . or is (20%) .>
      transform =: scale(0.9) <<>> rotate((-3)deg)

    is (30%) . or is (50%) . or is (70%) . or is (90%) .>
      transform =: scale(1.1) <<>> rotate(3deg)

    is (40%) . or is (60%) . or is (80%) .>
      transform =: scale(1.1) <<>> rotate((-3)deg)

    is (100%) .>
      transform =: scale(1) <<>> rotate(0)

instance Theme Tada where
    theme c = renderAnimationStyles "tada" tadaAnimation

data Pulse = Pulse
pulse = SomeInTheme Pulse
pulseAnimation = simpleStaticAnimation "pulse" 750 $ do
    is (0%) .> do
      transform =: scale(1)
      opacity   =: 1

    is (50%) .> do
      transform =: scale(0.9)
      opacity   =: 0.7

    is (100%) .> do
        transform =: scale(1)
        opacity   =: 1

instance Theme Pulse where
    theme c = renderAnimationStyles c pulseAnimation

data Jiggle = Jiggle
jiggle = SomeInTheme Jiggle
jiggleAnimation = simpleStaticAnimation "jiggle" 750 $ do
    let fs = [ (0,1,1,1), (30,1.25,0.75,1), (40,0.75,1.25,1), (50,1.15,0.85,1), (65,0.95,1.05,1), (75,1.05,0.95,1), (100,1,1,1) ]
    for_ fs $ \(p,x,y,z) -> 
        is (p%) .> 
            transform =: scale3d(x,y,z) 
instance Theme Jiggle where
    theme c = renderAnimationStyles c jiggleAnimation

data Glow = Glow
glow = SomeInTheme Glow
glowAnimation = StaticAnimationStyles 
    { animationStyles = defaultAnimationStyles

    , onTransition = void $ apply $ do
          animation-duration =: 2000 
          animation-name =: "glow"
          animation-timing-function =: cubez(0.19,1,0.22,1)

    , onAnimation = void $ atKeyframes "glow" $ do
        is (0%) .> 
          background-color =: hex 0xFCFCFD
        is (30%) .>
          background-color =: hex 0xFFF6CD
        is (100%) .>
          background-color =: hex 0xFCFCFD
    }
instance Theme Glow where
    theme c = renderAnimationStyles c glowAnimation