import qualified Data.Map as M
import Data.Monoid (mconcat)

import XMonad
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ICCCMFocus (takeTopFocus)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders (smartBorders)

main = xmonad myConfig

myConfig = XConfig
  { normalBorderColor = "#555753",
    focusedBorderColor = "#ef2929",
    terminal = myTerm,
    workspaces = workspaces gnomeConfig,
    layoutHook = (smartBorders . desktopLayoutModifiers) myLayout,
    manageHook = myManageHook >> manageHook gnomeConfig,
    handleEventHook = mconcat [ fullscreenEventHook, handleEventHook gnomeConfig ],
    modMask = mod4Mask,
    keys = \c -> myKeys c `M.union` keys gnomeConfig c,
    mouseBindings = mouseBindings gnomeConfig,
    borderWidth = 2,
    logHook = takeTopFocus >> logHook gnomeConfig,
    startupHook = startupHook gnomeConfig >> setSupported,
    focusFollowsMouse = True}

myTerm = "gnome-terminal --disable-factory"

myLayout = tiled ||| Mirror tiled ||| Grid ||| Full
  where tiled = Tall 1 (3/100) (1/2)

myKeys (XConfig {modMask = modm}) = M.fromList $
  [ ((modm, xK_p), spawn "dmenu_run"),
    ((modm .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock"),
    ((0, xK_Print), spawn "scrot"),
    ((mod1Mask, xK_Print), spawn "scrot -u")
    ]

myManageHook = do
  isFullscreen --> doFullFloat

-- Copied from XMonad.Hooks.EwmhDesktops with _NET_WM_STATE_FULLSCREEN appended.
setSupported :: X ()
setSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ,"_NET_WM_STATE_FULLSCREEN"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)
