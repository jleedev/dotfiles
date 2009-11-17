-- TODO update me for 0.9 release, basically getting rid of the handful of
-- functions I copy/pasted.

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral (spiral)
import Foreign.C.Types (CLong)
import qualified Data.Map as M

main = xmonad (withUrgencyHook NoUrgencyHook myConfig)

myConfig = desktopConfig
    { terminal = "x-terminal-emulator"
    , borderWidth = 1
    , focusFollowsMouse = False
    , layoutHook = (ewmhDesktopsLayout . avoidStruts . smartBorders) myLayout
    , modMask  = mod4Mask
    , focusedBorderColor = "#d70751"
    , normalBorderColor = "#eaeaea"
    , logHook = myLogHook >> logHook desktopConfig
    , manageHook = manageHook desktopConfig <+> myManageHook
    , keys     = \c -> myKeys c `M.union` keys desktopConfig c }

myLayout = tiled ||| Mirror tiled ||| spiral 1 ||| Grid ||| Full
	where tiled = Tall 1 (3/100) (1/2)

myKeys (XConfig {modMask = modm, terminal = terminal}) = M.fromList $
    [ ((modm,               xK_p),      spawn "dmenu_run")
    , ((modm,               xK_Return), spawn terminal)
    , ((modm .|. shiftMask, xK_q),      spawn "gnome-session-save --gui --kill")
    , ((0,                   xK_Print),  spawn "scrot")
    , ((mod1Mask,                   xK_Print),  spawn "scrot -u")
    , ((modm,               xK_s),      spawn "dmenu < docs/scrabble/words.txt")
    , ((modm .|. shiftMask, xK_s),      spawn "dmenu < docs/scrabble/owl2-lwl.txt")
    , ((modm,               xK_f),      focusUrgent)
    , ((modm,               xK_Down),  nextWS)
    , ((modm,               xK_Up),   prevWS)
    , ((modm .|. shiftMask, xK_Down),  shiftToNext >> nextWS)
    , ((modm .|. shiftMask, xK_Up),   shiftToPrev >> prevWS) ]

myManageHook = composeAll
    [ (isSplash <||> isToolbar) --> doIgnore
    , isFullscreen --> doFullFloat ]

myLogHook = do
    --updatePointer (Relative 0.5 0.5)
    return ()

----- Shamelessly ganked from XMonad.Hooks.ManageHelpers from the future:

isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
      <||> isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASHSCREEN"

isToolbar :: Query Bool
isToolbar = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_TOOLBAR"

-- | Helper to check if a window property contains certain value.
isInProperty :: String -> String -> Query Bool
isInProperty p v = ask >>= \w -> liftX $ do
    va <- getAtom v
    r <- getProp32s p w
    return $ case r of
        Just xs -> fromIntegral va `elem` xs
        _ -> False

-- | Get a window property from string
getProp32s :: String -> Window -> X (Maybe [CLong])
getProp32s str w = do { a <- getAtom str; getProp32 a w }

-- | Get a window property from atom
getProp32 :: Atom -> Window -> X (Maybe [CLong])
getProp32 a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w
