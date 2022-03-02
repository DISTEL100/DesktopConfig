import XMonad
import XMonad.Prelude

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
import XMonad.Util.NamedWindows( getName, getNameWMClass )

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Hidden
import XMonad.Layout.Magnifier
import XMonad.Layout.Gaps
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
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.DynamicProperty

import XMonad.Actions.WindowBringer
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
    , borderWidth        = 1
    } `additionalKeysP` myKeys
    
-- ############################################################################
--                           EVENT HOOK
-- ############################################################################
myEventHook = hintsEventHook <+> fadeWindowsEventHook

-- ############################################################################
--                           LOG HOOK
-- ############################################################################
myLogHook = historyHook <+> fadeWindowsLogHook myFadeHook
myFadeHook = composeAll 
   [ 
    opaque
    , stringProperty "_XMONAD_TAGS" =? "drawer" --> transparency 0.15
   ]

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
    $ myBSP ||| myTallNoMag ||| myFull ||| my3Col

myBSP = renamed [ Replace "BSP" ] 
    $ layoutHintsWithPlacement (0, 0) 
    $ myDrawer
    $ hiddenWindows
    $ mySpacing
    $ MT.mkToggle (MT.single FULL)
    $ emptyBSP
my3ColNoMag = renamed [ Replace "3ColNoMag" ]
    $ layoutHintsWithPlacement (0, 0) 
    $ hiddenWindows
    $ myDrawer
    $ mySpacing
    $ maximizeWithPadding 30
    $ ThreeColMid 1 (3/100) (1/2)
my3Col = renamed [ Replace "3Col" ]
    $ layoutHintsWithPlacement (0, 0) 
    $ hiddenWindows
    $ myDrawer
    $ mySpacing
    $ magnifiercz' 1.5 
    $ maximizeWithPadding 30
    $ ThreeColMid 1 (3/100) (1/2)
myTall = renamed [ Replace "Tall" ]
    $ layoutHintsWithPlacement (0, 0) 
    $ hiddenWindows
    $ myDrawer
    $ mySpacing
    $ magnifiercz' 1.3 
    $ maximizeWithPadding 30
    $ MT.mkToggle (MT.single FULL)
    $ Tall 1 (3/100) (11/18)
myTallNoMag = renamed [ Replace "TallNoMag" ]
    $ hiddenWindows
    $ layoutHintsWithPlacement (0, 0) 
    $ myDrawer
    $ mySpacing
    $ MT.mkToggle (MT.single FULL)
    $ Tall 1 (3/100) (1/2)
myFull = smartBorders
    $ Full

myDrawer =  onBottom $ drawer 0.0 0.8 (Tagged "drawer") myDrawerLayout
myDrawerLayout = spacingRaw False (Border 1400 0 0 0) True (Border 0 0 0 0) True $ Tall 0 00.3 0.6
mySpacing = spacingRaw True (Border 2 0 2 0) True (Border 0 2 0 2) True

data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ W.floating wset

-- ############################################################################
--                           ACTIONS
-- ############################################################################
windowBringerConf :: WindowBringerConfig
windowBringerConf = def { 
    menuArgs = [ "-l", "20"
               , "-i"
               , "-b"
               , "-nb", colInactive
               , "-nf", colFg
               , "-sb", colSep
               , "-sf", colBrown
               ],
    windowTitler = \ws -> \w -> do
          name <- show <$> getName w
          className <- show <$> getNameWMClass w
          return $ "  " ++ W.tag ws
                        ++ "   |   " 
                        ++ (fixedWidth 16 className) 
                        ++ "   |   "
                        ++ name
    }
fixedWidth :: Int -> String -> String
fixedWidth 0 s      = ""
fixedWidth x ""     = " " ++ (fixedWidth (x-1) "") 
fixedWidth x (s:sw) = [s] ++ (fixedWidth (x-1) sw)

spawnPrograms = [
              ("1Password",     "1password")
            , ( "Pulsemixer",   "xterm -bg DarkBlue -class xterm_gridSelect -e pulsemixer" )
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

spawnXtermInPath = withFocused (\win -> do 
-               wm_name  <- runQuery title win
-               wm_class <- runQuery className win
-                if wm_class == "XTerm"
-                   then spawn $ "xterm -e 'cd " ++ extractPathFromTitle wm_name ++ " && bash'"
-                   else spawn "xterm"
-                  )
-extractPathFromTitle :: String -> String
-extractPathFromTitle = foldl (\s -> \c -> if c == ':' then "" else s ++ [c] ) ""
-- ############################################################################
--                           ON STARTUP
-- ############################################################################
myStartupHook = do
        spawnOnOnce "1" "xterm"
        spawnOnOnce "9" "thunderbird"
        spawnOnOnce "9" "notion-snap"
        spawnOnOnce "9" "slack"
        spawn "hsetroot -solid black"

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
    , ("M-h",           sendMessage ( ExpandTowards L) >> sendMessage  Shrink                   )
    , ("M-S-h",         sendMessage ( ExpandTowards D) )
    , ("M-l",           sendMessage ( ExpandTowards R ) >> sendMessage Expand                      )
    , ("M-S-l",         sendMessage ( ExpandTowards U ) )
    , ("M-n",           windows W.swapMaster                    )
    , ("M-S-p",         unGrab *> spawn "scrot -s"                  )
    , ("M-f",           runOrRaiseMaster "firefox" (className =? "firefox") )
    , ("M-c",           kill                                        )
    , ("M-r",           renameWorkspace def                         )
    , ("M-S-r",         changeDir def                               )
    , ("M-s",           gridselect configSystem spawnSystem >>= spawn . fromMaybe "" )
    , ("M-a",           gridselect configPrograms spawnPrograms >>= spawn . fromMaybe "" )
    , ("M-d",           goToSelected def                            )
    , ("M-x",           withFocused $ sendMessage . maximizeRestore )
    , ("M1-<Tab>",      toggleRecentNonEmptyWS                      )
    , ("M-u",           focusUrgent                                 )
    , ("M-<L>",         Cyc.moveTo Prev ((Cyc.Not emptyWS) :&: hiddenWS)           )
    , ("M-<R>",         Cyc.moveTo Next ((Cyc.Not emptyWS) :&: hiddenWS)           )
    , ("M-S-y",         moveToDrawer                                )
    , ("M-y",           focusUpTagged "drawer"                      )
    , ("M1-b",          bringMenuConfig windowBringerConf                      )
    , ("M1-g",          gotoMenuConfig windowBringerConf                      )
    , ("M1-h",          withFocused hideWindow   )
    , ("M1-S-h",        popOldestHiddenWindow  )
    , ("M-<Return>",    spawnXtermInPath                          )
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
