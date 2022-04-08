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
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows( getName, getNameWMClass )
import XMonad.Util.Themes

import XMonad.Layout.Hidden
import XMonad.Layout.DwmStyle
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Master
import XMonad.Layout.Reflect
import XMonad.Layout.Magnifier
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Gaps
import XMonad.Layout.TwoPane
import qualified XMonad.Layout.ComboP as CP
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
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
import qualified XMonad.Layout.GridVariants as GV
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.DynamicProperty

import XMonad.Actions.ShowText
import XMonad.Actions.WindowBringer
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS as Cyc
import XMonad.Actions.CycleWindows
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
colInactive = "#42403d"
colBrown    = "#936e9c"
colUrgent   = "#f58402"
colHigh     = "#ff0073"
colSep      = "#2b2b2b" 
colFg       = "snow2"
colBg       = "#202922"
colBg2      = "#333333"
colBlack    = "#161716"
colGray     = "#74807b"
colGray2    = "#999999"
font        = "xft:IBMPlexMono:size=9:style=italic"

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
myEventHook = hintsEventHook <+> fadeWindowsEventHook <+> handleTimerEvent

-- ############################################################################
--                           LOG HOOK
-- ############################################################################
fadedOpacity = 0.1
myLogHook = historyHook <+> fadeWindowsLogHook myFadeHook
myFadeHook = composeAll 
   [ 
    opaque
    , title     =? "nnn"               --> transparency fadedOpacity
    , className =? "1Password"         --> transparency fadedOpacity
   ]

-- ############################################################################
--                           MANAGE HOOK
-- ############################################################################
myManageHook :: ManageHook
myManageHook = ( namedScratchpadManageHook myScratchpads )
   <+> manageSpawn 
   <+> composeAll
    [ className =? "Gimp"              --> doFloat
    , className =? "SuperCollider"     --> doShift "4"
    , className =? "1Password"         --> doCenterFloat
    , className =? "Xmessage"          --> doCenterFloat
    , appName   =? "Devtools"          --> doCenterFloat
    , className =? "xterm_gridSelect"  --> doRectFloat (W.RationalRect (1/4) (1/4) (1/2) (1/2))
    , className =? "Thunderbird"       --> doShift "9"
    , appName   =? "Calendar"          --> doRectFloat (W.RationalRect (1/4) (1/4) (1/2) (1/2))
    , isFullscreen                     --> doFullFloat
    , isDialog                         --> doCenterFloat
    , className =? "Alert"             --> doAskUrgent
    , title     =? "preview-tui"       --> doF W.focusDown
    ]
nspGapH = (1/35)
nspGapV = nspGapH * (16/9)
nnnW = (1/3) - (1.5 * nspGapH)
nnnH = (1/2) - (1.5 * nspGapV)
nnnPos =  customFloating (W.RationalRect nspGapH nspGapV nnnW nnnH )
passW = (2/3) - (2 * nspGapH)
passH = 1 - (2*nspGapV)
passPos = customFloating (W.RationalRect ((2*nspGapH)+nnnW) nspGapV passW passH )
myScratchpads = [ 
	  NS "nnn" ("xterm -bg Orange4 -e 'nnn' ") (title =? "nnn") nnnPos
        , NS "1Password" "1password" (className =? "1Password") passPos
		]

-- ############################################################################
--                           LAYOUTS
-- ############################################################################
myLayout = onWorkspace "9" myFull
    $ workspaceDir "~"
    $ lessBorders AllFloats 
    $ myModifiers
    $ windowNavigation
    $ myMasterGrid ||| myGrid 

myMasterGrid = renamed [ Replace "MGrid" ]
    $ hiddenWindows
    $ smartBorders
    $ mastered (1/100) (10/24) $ GV.Grid (15/13)
myGrid = renamed [ Replace "Grid" ]
    $ hiddenWindows
    $ smartBorders
    $ dwmStyle shrinkText mySDConfig
    $ GV.Grid (978/1057)

myFull = smartBorders
    $ Full

mySDConfig = def { inactiveTextColor   = colActive
		 , activeColor         = colActive
		 , inactiveColor       = colBlack
		 , urgentColor 	       = colUrgent
		 , activeBorderWidth   = 0
		 , inactiveBorderWidth = 0
		 , decoHeight          = 22
		 , decoWidth           = 190
		 , fontName            = font
		 }

myModifiers = MT.mkToggle (MIRROR MT.?? FULL MT.?? NOBORDERS MT.?? MT.EOT)

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

toggleTag tag win = do
	b <- (hasTag tag win)
        if b 
	   then delTag tag win
	   else addTag tag win

spawnXtermInPath = withFocused (\win -> do 
		wm_name  <- runQuery title win
		wm_class <- runQuery className win
                if wm_class == "XTerm"
                   then spawn $ "xterm -e 'cd " ++ extractPathFromTitle wm_name ++ " && bash'"
                   else spawn "xterm"
                  )
extractPathFromTitle :: String -> String
extractPathFromTitle = foldl (\s -> \c -> if c == ':' then "" else s ++ [c] ) ""
-- ############################################################################
--                           ON STARTUP
-- ############################################################################
myStartupHook = do
        spawnOnce "xsetroot -solid Black"
        spawnOnce "picom"
        spawnOnOnce "9" "thunderbird"

-- ############################################################################
--                           KEYBINDINGS
-- ############################################################################
myKeys = [ 
      ("M-z",        spawn "slock"                                       )
    , ("M-x",        sendMessage (MT.Toggle FULL)  )
    , ("M-S-x",      sendMessage $ MT.Toggle MIRROR                        )
    , ("M-m",        windows W.focusMaster                 )
    , ("M-n",        windows W.swapMaster                    )
    , ("M-S-y",      namedScratchpadAction myScratchpads "nnn"        )
    , ("M-y",        namedScratchpadAction myScratchpads "1Password"  )
    , ("M1-<L>",     Cyc.moveTo Prev relevantWorkspaces)
    , ("M1-<R>",     Cyc.moveTo Next relevantWorkspaces)
    , ("M1-h",       Cyc.moveTo Prev relevantWorkspaces)
    , ("M1-l",       Cyc.moveTo Next relevantWorkspaces)
    , ("M1-S-<L>",   Cyc.shiftTo Prev relevantWorkspaces >> Cyc.moveTo Prev relevantWorkspaces)
    , ("M1-S-<R>",   Cyc.shiftTo Next relevantWorkspaces >> Cyc.moveTo Next relevantWorkspaces)
    , ("M1-S-h",     Cyc.shiftTo Prev relevantWorkspaces >> Cyc.moveTo Prev relevantWorkspaces)
    , ("M1-S-l",     Cyc.shiftTo Next relevantWorkspaces >> Cyc.moveTo Next relevantWorkspaces)
    , ("M1-<Tab>",   cycleRecentNonEmptyWS [xK_Alt_L] xK_Tab xK_q)
    , ("M-<Tab>",    windows W.focusUp )
    , ("M-S-<Tab>",  windows W.focusDown )
    , ("<Page_Up>",  nextMatch History (return True) )
    , ("<Page_Down>",nextMatchWithThis Forward className )
    , ("M-j",        sendMessage $ Go D                            )
    , ("M-k",        sendMessage $ Go U                          )
    , ("M-h",        sendMessage $ Go L                          )
    , ("M-l",        sendMessage $ Go R                          )
    , ("M-S-j",      sendMessage $ Swap D                         )
    , ("M-S-k",      sendMessage $ Swap U                           )
    , ("M-S-h",      sendMessage $ Swap L                           )
    , ("M-S-l",      sendMessage $ Swap R                           )
    , ("M-<D>",      sendMessage $ Go D                            )
    , ("M-<U>",      sendMessage $ Go U                          )
    , ("M-<L>",      sendMessage $ Go L                          )
    , ("M-<R>",      sendMessage $ Go R                          )
    , ("M-S-<D>",    sendMessage $ Swap D                         )
    , ("M-S-<U>",    sendMessage $ Swap U                           )
    , ("M-S-<L>",    sendMessage $ Swap L                           )
    , ("M-S-<R>",    sendMessage $ Swap R                           )
    , ("M--",        sendMessage Shrink                   )
    , ("M-+",        sendMessage Expand                      )
    , ("<Print>",    unGrab *> spawn "scrot -s"                  )
    , ("M-f",        runOrRaiseMaster "firefox" (className =? "firefox") )
    , ("M-S-f",      ifWindow (className =? "firefox") (currentWs >>= doShift) idHook)
    , ("M-c",        kill                                        )
    , ("M-r",        renameWorkspace def                         )
    , ("M-S-r",      changeDir def                               )
    , ("M-s",        gridselect configSystem spawnSystem >>= spawn . fromMaybe "" )
    , ("M-a",        gridselect configPrograms spawnPrograms >>= spawn . fromMaybe "" )
    , ("M-u",        focusUrgent)
    , ("M1-b",       bringMenuConfig windowBringerConf)
    , ("M1-g",       gotoMenuConfig windowBringerConf)
    , ("M1-v",       withFocused hideWindow )
    , ("M1-S-v",     popOldestHiddenWindow )
    , ("M-<Return>", spawn "xterm" )
    ] 

relevantWorkspaces =  (( Cyc.Not emptyWS ) :&: hiddenWS :&: ignoringWSs ["NSP"] )

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
