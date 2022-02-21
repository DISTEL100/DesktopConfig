{-# LANGUAGE TupleSections #-}

import XMonad
import Control.Monad
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

import XMonad.Layout.Groups.Examples

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Maximize
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing
import XMonad.Layout.Drawer
import XMonad.Layout.LayoutHints
import qualified XMonad.Layout.BoringWindows as Boring
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicIcons

import XMonad.Actions.WindowBringer
import XMonad.Actions.ShowText
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS as Cyc
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.TagWindows
import XMonad.Actions.GroupNavigation

import Data.Maybe (fromMaybe)
import qualified Data.Map as M

-- ############################################################################
--                           COLORS & FONTS
-- ############################################################################
colActive   = "LightGreen"
colInactive = "#45363d"
colBrown    = "#936e9c"
colUrgent   = "#f58402"
colHigh     = "#ff0073"
colSep      = "#2b2b2b" 
colFg       = "snow2"
colBg       = "#202922"
colBlack    = "#161716"
colGray     = "#74807b"

-- ############################################################################
--                           MAIN
-- ############################################################################
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
--                           CONFIG
-- ############################################################################
myConfig = def
    { modMask            = mod4Mask      
    , layoutHook         = myLayout     
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , startupHook        = myStartupHook
    , logHook            = myLogHook
    , normalBorderColor  = colInactive
    , focusedBorderColor = colActive
    , borderWidth        = 2
    } `additionalKeysP` myKeys
    
-- ############################################################################
--                           EVENT HOOK
-- ############################################################################
myEventHook = hintsEventHook <+> handleTimerEvent

-- ############################################################################
--                           LOG HOOK
-- ############################################################################
myLogHook = historyHook

-- ############################################################################
--                           MANAGE HOOK
-- ############################################################################
myManageHook :: ManageHook
myManageHook = manageSpawn <+> composeAll
    [ className =? "Gimp"              --> doFloat
    , className =? "SuperCollider"     --> doShift "4"
    , className =? "1Password"         --> doCenterFloat
    , className =? "xterm_gridSelect"  --> doRectFloat (W.RationalRect (1/4) (1/4) (1/2) (1/2))
    , className =? "Thunderbird"       --> doShift "9"
    , appName   =? "Calendar"          --> doRectFloat (W.RationalRect (1/4) (1/4) (1/2) (1/2))
    , isFullscreen                     --> doFullFloat
    , isDialog                         --> doCenterFloat
    , className =? "Alert"             --> doAskUrgent
    , title     =? "preview-tui"       --> doF W.focusDown
    ]

-- ############################################################################
--                           LAYOUTS
-- ############################################################################
myLayout = Boring.boringWindows
    $ onWorkspace "9" myFull
    $ workspaceDir "~"
    $ lessBorders AllFloats 
    $ myTall ||| myTallNoMag ||| myFull ||| my3Col ||| my3ColNoMag  

my3ColNoMag = renamed [ Replace "3ColNoMag" ]
    $ layoutHintsWithPlacement (0.5, 0.5) 
    $ myDrawer
    $ mySpacing
    $ maximizeWithPadding 30
    $ ThreeColMid 1 (3/100) (1/2)
my3Col = renamed [ Replace "3Col" ]
    $ layoutHintsWithPlacement (0.5, 0.5) 
    $ myDrawer
    $ mySpacing
    $ magnifiercz' 1.5 
    $ maximizeWithPadding 30
    $ ThreeColMid 1 (3/100) (1/2)
myTall = renamed [ Replace "Tall" ]
    $ layoutHintsWithPlacement (0.5, 0.5) 
    $ myDrawer
    $ smartBorders
    $ mySpacing
    $ magnifiercz' 1.3 
    $ maximizeWithPadding 30
    $ MT.mkToggle (MT.single FULL)
    $ Tall 1 (3/100) (11/18)
myTallNoMag = renamed [ Replace "TallNoMag" ]
    $ layoutHintsWithPlacement (0.5, 0.5) 
    $ myDrawer
    $ smartBorders
    $ mySpacing
    $ maximizeWithPadding 15
    $ MT.mkToggle (MT.single FULL)
    $ Tall 1 (3/100) (1/2)
myFull = smartBorders
    $ Full

myDrawer = onRight $ simpleDrawer 0.0 0.2 (Tagged "drawer") 
mySpacing = spacingRaw True (Border 2 0 2 0) True (Border 0 2 0 2) True

data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ W.floating wset

-- ############################################################################
--                           ACTIONS
-- ############################################################################

textTest =  ["a", "b", "c"]

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

toggleTagBoring tag win = do b <- hasTag tag win
                             if b then Boring.clearBoring >> delTag tag win
                                  else Boring.markBoring  >> addTag tag win

moveToDrawer = withFocused (toggleTagBoring "drawer") 
                >> nextMatch History (return True)
-- ############################################################################
--                           ON STARTUP
-- ############################################################################
myStartupHook = do
        spawnOnOnce "1" "xterm"
        spawnOnOnce "9" "thunderbird"
        spawnOnOnce "9" "signal-desktop"
        spawnOnOnce "9" "telegram-desktop"
        spawn "picom"

-- ############################################################################
--                           KEYBINDINGS
-- ############################################################################
myKeys = [ 
      ("M-z",           spawn "slock"                                       )
    , ("M1-f",          sendMessage $ MT.Toggle FULL                        )
    , ("M-<Tab>",       Boring.focusDown                                    )
    , ("M-j",           Boring.focusDown                            )
    , ("M-S-j",         Boring.swapDown                         )
    , ("M-S-<Tab>",     Boring.focusUp                          )
    , ("M-k",           Boring.focusUp                          )
    , ("M-S-k",         Boring.swapUp                           )
    , ("M-m",           Boring.focusMaster                      )
    , ("M-n",           windows W.swapMaster                    )
    , ("M-S-=",         unGrab *> spawn "scrot -s"                  )
    , ("M-f",           runOrRaiseMaster "firefox" (className =? "firefox") )
    , ("M-<Return>",    spawn "xterm"                          )
    , ("M-c",           kill                                        )
    , ("M-r",           renameWorkspace def                         )
    , ("M-S-r",         changeDir def                               )
    , ("M-s",           gridselect configSystem spawnSystem >>= spawn . fromMaybe "" )
    , ("M-a",           gridselect configPrograms spawnPrograms >>= spawn . fromMaybe "" )
    , ("M-d",           goToSelected def                            )
    , ("M-x",           withFocused $ sendMessage . maximizeRestore )
    , ("M1-<Tab>",      toggleRecentNonEmptyWS                      )
    , ("M-u",           focusUrgent                                 )
    , ("M-<L>",         Cyc.moveTo Prev (Cyc.Not emptyWS)           )
    , ("M-<R>",         Cyc.moveTo Next (Cyc.Not emptyWS)           )
    , ("M-S-h",         Cyc.moveTo Prev (Cyc.Not emptyWS)           )
    , ("M-S-l",         Cyc.moveTo Next (Cyc.Not emptyWS)           )
    , ("M-S-y",         moveToDrawer                                )
    , ("M-y",           focusUpTagged "drawer"                      )
    , ("M1-i",           zoomWindowOut                          )
    , ("M1-u",           zoomWindowIn                      )
    , ("M1-o",           toggleColumnFull                      )
    , ("M1-z",           splitGroup                      )
    , ("M1-b",           gotoMenu                      )
    , ("M1-g",           bringMenu                      )
    , ("M1-c",           flashText def 1 (textTest)                      )
    ]

-- ############################################################################
--                           XMOBAR SETTINGS
-- ############################################################################
myStatusBar = withEasySB (statusBarProp "xmobar" myXmobarPP) defToggleStrutsKey

myXmobarPP = workspaceNamesPP def
    { ppSep             = xmobarColor colActive "" ""
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = xmobarBorder "Full" "LightBlue mb=1" 4 . xmobarColor "LightBlue" colSep . wrap "  " "  " 
    , ppVisible         = xmobarBorder "Full" "Yellow mb=1   " 4 . xmobarColor "Yellow" colSep . wrap "  " "  " 
    , ppUrgent          = wrap "  " "  " . xmobarBorder "VBoth" colUrgent 4 
    , ppHidden          = xmobarColor colFg colSep . wrap "  " "  "
    , ppHiddenNoWindows = \x -> ""
    , ppLayout          = xmobarBorder "Full" colSep 2 . xmobarColor colFg colSep . wrap "" "   "
    , ppOrder           = \[ws, l, _, wins] -> [   
                                xmobarBorder "Full" colSep 0 $ xmobarColor colFg colSep ("  " ++ ws ++ "  ")
                              , l
                              , wins
                            ]
    , ppExtras          = [ onLogger (wrap "    " "" ) windowTitles ]
    } 
  where
    windowTitles    = logTitles formatFocused formatUnfocused
    formatFocused   = wrap " " " " . xmobarBorder "Top" colActive 2 . xmobarColor colActive "" . ppWindow
    formatUnfocused = xmobarColor colFg "" . wrap " " " " . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30
