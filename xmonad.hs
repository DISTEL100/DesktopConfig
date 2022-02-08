import XMonad

import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Maximize
import XMonad.Layout.PerWorkspace
import XMonad.Layout.TabBarDecoration

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicIcons

import XMonad.Actions.WorkspaceNames
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS as Cyc

import Data.Maybe (fromMaybe)
import qualified Data.Map as M

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . workspaceNamesEwmh
     . setEwmhActivateHook doAskUrgent
     . ewmh
     . myStatusBar
     $ withUrgencyHook NoUrgencyHook
     $ myConfig

-- ############################################################################
--                           COLORS & FONTS
-- ############################################################################
colActive   = "LightGreen"
colInactive = "#45363d"
colBrown    = "#5e3535"
colUrgent   = "#f58402"
colHigh     = "#ff0073"
colSep      = "DarkGoldenrod" 
colFg       = "snow2"
colBg       = "#202922"
colBlack    = "#161716"

-- ############################################################################
--                           CONFIG
-- ############################################################################
myConfig = def
    { modMask            = mod4Mask      
    , layoutHook         = myLayout     
    , manageHook         = myManageHook
    , startupHook        = myStartupHook
    , normalBorderColor  = colBrown
    , focusedBorderColor = colActive
    , borderWidth        = 2
    } `additionalKeysP` myKeys
    
-- ############################################################################
--                           MANAGE HOOK
-- ############################################################################
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"              --> doFloat
    , className =? "SuperCollider"     --> doShift "4"
    , className =? "1Password"         --> doCenterFloat
    , className =? "xterm_gridSelect"  --> doRectFloat (W.RationalRect (1/4) (1/4) (1/2) (1/2))
    , className =? "Thunderbird"       --> doShift "9"
    , appName   =? "Calendar"          --> doRectFloat (W.RationalRect (1/4) (1/4) (1/2) (1/2))
    , isFullscreen                     --> doFullFloat
    , isDialog                         --> doCenterFloat
    ]

-- ############################################################################
--                           LAYOUTS
-- ############################################################################
myLayout = onWorkspace "9" myFull
    $ lessBorders AllFloats 
    $ myTall ||| myFull ||| my3Col

my3Col = renamed [ Replace "3Col" ]
    $ smartBorders
    $ magnifiercz' 1.5 
    $ maximizeWithPadding 30
    $ ThreeColMid 1 (3/100) (1/2)
myTall = renamed [ Replace "Tall" ]
    $ smartBorders
    $ magnifiercz 1.02 
    $ maximizeWithPadding 30
    $ Tall 1 (3/100) (1/2)
myFull = smartBorders
    $ Full

data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ W.floating wset

-- ############################################################################
--                           ACTIONS
-- ############################################################################
spawnPrograms = [
              ("1Password",     "1password")
            , ( "Bluetooth",    "xterm -bg DarkBlue -class xterm_gridSelect -e bluetoothctl" )
            , ( "Pulsemixer",   "xterm -bg DarkBlue -class xterm_gridSelect -e pulsemixer" )
            , ( "iwd",          "xterm -bg DarkBlue -class xterm_gridSelect -e iwctl" )
            , ( "xterm",        "xterm -bg DeepPink4 -class xterm_gridSelect" )
            , ( "ghci",         "xterm -bg DeepSkyBlue4 -class xterm_gridSelect -e 'stack repl'" )
            ]
configPrograms :: GSConfig String
configPrograms = def { 
                   gs_cellheight = 80
                 , gs_cellwidth = 300
                 , gs_cellpadding = 20
                 , gs_font = "xft:IBMPlexMono:size=11:style=italic"
                 , gs_bordercolor = "snow2"
                 }
spawnSystem = [
      ( "Hibernate", "systemctl hibernate" )
    , ( "Shutdown", "shutdown 0" )
    , ( "Hybrid-Sleep", "systemctl hybrid-sleep" )
    , ( "Backup", "xterm -bg DarkBlue -class xterm_gridSelect -e 'sudo backupWhiteRiffle'" )
    , ( "Journal", "xterm -bg DarkBlue -class xterm_gridSelect -e 'journalctl -r'" )
    , ( "Bildschirm ausschalten", "xset dpms force off" )
    , ( "Suspend", "systemctl suspend" )
    , ( "restart iwd", "xterm -bg DarkBlue -class xterm_gridSelect -e 'sudo systemctl restart iwd.service'" )
    , ( "System Update", "xterm -bg DarkBlue -class xterm_gridSelect -e 'pacman -Qqen > ~/.pkglist_Qqen.txt && pacman -Qqem > ~/.pkglist_Qqem.txt && sudo pacman -Syu'" )
    ]
configSystem :: GSConfig String
configSystem = def { 
                   gs_cellheight = 80
                 , gs_cellwidth = 300
                 , gs_cellpadding = 20
                 , gs_font = "xft:IBMPlexMono:size=11:style=italic"
                 , gs_bordercolor = "snow2"
                 }

-- ############################################################################
--                           ON STARTUP
-- ############################################################################
myStartupHook = do
        spawnOnOnce "1" "xterm"
        spawnOnOnce "9" "thunderbird"
        spawn "picom"

-- ############################################################################
--                           KEYBINDINGS
-- ############################################################################
myKeys = [ 
      ("M-S-l", spawn "slock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        )
    , ("M-]"  , spawn "firefox"                   )
    , ("M-r"  , renameWorkspace def               )
    , ("M-s"  , gridselect configSystem spawnSystem >>= spawn . fromMaybe ""      )
    , ("M-a"  , gridselect configPrograms spawnPrograms >>= spawn . fromMaybe ""      )
    , ("M-d"  , goToSelected def                  )
    , ("M-x"  , withFocused $ sendMessage . maximizeRestore )
    , ("M-<L>", Cyc.moveTo Prev (Cyc.Not emptyWS  ) )
    , ("M-<R>", Cyc.moveTo Next (Cyc.Not emptyWS  ) )
    , ("M-S-h", Cyc.moveTo Prev (Cyc.Not emptyWS  ) )
    , ("M-S-l", Cyc.moveTo Next (Cyc.Not emptyWS  ) )
    ]

-- ############################################################################
--                           XMOBAR SETTINGS
-- ############################################################################
myStatusBar = withEasySB (statusBarProp "xmobar" myXmobarPP) defToggleStrutsKey

myXmobarPP = workspaceNamesPP def
    { ppSep             = wrap " " " " $ xmobarColor colActive "" " "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = xmobarColor colFg "" . wrap "" "" . xmobarBorder "Top" "#fffc3b" 3
    , ppVisible         = xmobarColor colFg "" . wrap "" "" . xmobarBorder "Top" colBrown 3
    , ppHidden          = xmobarColor colFg "" . wrap "" ""
    , ppHiddenNoWindows = xmobarColor colInactive "" . wrap "" ""
    , ppLayout          = wrap "" "" . xmobarColor colFg ""
    , ppUrgent          = wrap "" "" . xmobarBorder "VBoth" colUrgent 2
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [ windowTitles ]
    } 
  where
    windowTitles    = logTitles formatFocused formatUnfocused
    formatFocused   = xmobarColor colBg colActive . wrap "  " "  " . ppWindow
    formatUnfocused = xmobarColor colFg  "#40324a" . wrap "  " "  " . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30
