--------------------------------------------------------------------------------
module Main where


--------------------------------------------------------------------------------
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Monoid                 (appEndo)


--------------------------------------------------------------------------------
import           XMonad
import           XMonad.Actions.CycleWS      (nextWS, prevWS, shiftToNext,
                                              shiftToPrev)
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.ManageDocks    (ToggleStruts (..), avoidStruts,
                                              manageDocks)
import           XMonad.Hooks.ManageHelpers  (doFullFloat, doRectFloat)
import           XMonad.Hooks.DynamicLog     (xmobar)
import           XMonad.Layout.Circle        (Circle (..))
import           XMonad.Layout.NoBorders     (smartBorders)
import           XMonad.Layout.PerWorkspace  (onWorkspace)
import           XMonad.Layout.SimplestFloat (simplestFloat)
import           XMonad.StackSet             (RationalRect (..), currentTag)
import           XMonad.Prompt               (defaultXPConfig)
import           XMonad.Prompt.Shell         (shellPrompt)


--------------------------------------------------------------------------------
main :: IO ()
main = xmonad =<< xmobar myConfig
  where
    myConfig = ewmh $ defaultConfig
        { terminal           = "urxvt"
        , modMask            = mod4Mask
        , manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , workspaces         = ["Play", "Work", "View", "Float"]
        , borderWidth        = 2
        , normalBorderColor  = "#002b36"
        , focusedBorderColor = "#688cb3"
        , keys               = \c -> myKeys c `M.union` keys defaultConfig c
        , focusFollowsMouse  = False
        }


--------------------------------------------------------------------------------
-- | Layout hook
myLayoutHook = smartBorders $ avoidStruts $
    onWorkspace "Play" (Mirror tiled ||| tiled ||| Circle) $
    onWorkspace "Work" (Mirror tiled ||| tiled ||| Full) $
    onWorkspace "View" (Mirror tiled ||| tiled ||| Full) $
    onWorkspace "Float" simplestFloat $ Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 3/100
    ratio = 1/2


--------------------------------------------------------------------------------
-- | Manage hook
myManageHook :: ManageHook
myManageHook = manageDocks <+> manageHook defaultConfig <+>
    (onFloatingWorkspace --> doFloat)


--------------------------------------------------------------------------------
-- | Check if a window is on a floating space
onFloatingWorkspace :: Query Bool
onFloatingWorkspace = liftX $
    withWindowSet (return . (`elem` floating) . currentTag)
  where
    floating = ["Float"]


--------------------------------------------------------------------------------
-- A list of custom keys
myKeys :: XConfig Layout -> Map (ButtonMask, KeySym) (X ())
myKeys (XConfig {modMask = myModMask}) = M.fromList $
    [ -- Some programs
      ((myModMask, xK_F1), spawn "firefox")
    , ((myModMask, xK_F2), spawn "thunar")
    , ((myModMask, xK_F3), spawn "rhythmbox")

      -- Logout
    -- , ((myModMask .|. shiftMask, xK_q), spawn "gnome-session-quit")
    , ((myModMask .|. shiftMask, xK_l), spawn "xscreensaver-command --lock")

      -- launcher keys
    , ((myModMask, xK_p), spawn "dmenu_run")

      -- Toggle struts
    , ((myModMask, xK_a), sendMessage ToggleStruts)

      -- Full float
    , ((myModMask, xK_f), fullFloatFocused)

      -- Centered rectangle float
    , ((myModMask, xK_r), rectFloatFocused)

      -- Shrik and expand
    , ((myModMask, xK_Right), sendMessage Expand)
    , ((myModMask, xK_Left),  sendMessage Shrink)

      -- XF86AudioLowerVolume
    , ((0, 0x1008ff11), spawn "amixer set Master 10%-")
      -- XF86AudioRaiseVolume
    , ((0, 0x1008ff13), spawn "amixer set Master 10%+")
      -- XF86AudioMute
    , ((0, 0x1008ff12), spawn "amixer set Master toggle")
      -- XF86AudioNext
    , ((0, 0x1008ff17), spawn "rhythmbox-client --next")
      -- XF86AudioPrev
    , ((0, 0x1008ff16), spawn "rhythmbox-client --previous")
      -- XF86AudioPlay
    , ((0, 0x1008ff14), spawn "rhythmbox-client --play-pause")
      -- XF86AudioStop
    , ((0, 0x1008ff15), spawn "rhythmbox-client --stop")
    ]
  where
    -- Function to fullFloat a window
    fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery
                          doFullFloat f

    -- Function to rectFloat a window
    rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery
                          (doRectFloat $ RationalRect 0.02 0.05 0.96 0.9) f
