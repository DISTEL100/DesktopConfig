{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
import XMonad
import XMonad.Prelude

import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.DynamicProperty
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

import XMonad.Layout.LayoutModifier
import XMonad.Layout.Hidden
import XMonad.Layout.DwmStyle
import XMonad.Layout.WindowArranger
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Groups as G
import XMonad.Layout.Groups.Helpers as GH
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow
import XMonad.Layout.Master
import XMonad.Layout.MultiColumns
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
import qualified XMonad.Layout.BinarySpacePartition as BSP
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

import XMonad.Actions.UpdatePointer
import XMonad.Actions.ShowText
import XMonad.Actions.Navigation2D as N2D
import XMonad.Actions.WindowBringer
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS as Cyc
import XMonad.Actions.CycleWindows
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.TagWindows
import XMonad.Actions.GroupNavigation

import Data.Maybe ( fromMaybe )
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
colBlack    = "#161716"
colGray     = "#74807b"
font size   = "xft:IBMPlexMono:size="++ size ++ ":style=italic"

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
myLogHook = historyHook <+> fadeWindowsLogHook myFadeHook >> updatePointer  (0.25, 0.25) (0.25, 0.25)
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
ncpamixerPos =  customFloating (W.RationalRect nspGapH (nspGapV+nnnH+nspGapV) (nnnW*2) nnnH )
myScratchpads = [ 
                   NS "nnn" ("xterm  -T \"nnn\" -e \'bash --init-file <(echo \". ~/.bashrc; nnn\")\' ") (title =? "nnn") nnnPos
                 , NS "1Password" "1password" (className =? "1Password") passPos
                 , NS "Mixer" "xterm -e 'ncpamixer'" ((stringProperty "WM_NAME") =? "ncpamixer") ncpamixerPos
                ]

-- ############################################################################
--                           LAYOUTS
-- ############################################################################
myLayout = onWorkspace "9" myFull
    $ workspaceDir "~"
    $ lessBorders AllFloats 
    $ myModifiers
    $ smartBorders
    $ windowNavigation
    $ myGroup ||| myGrid ||| zoomRow 

myGroup = renamed [ Replace "Groups" ]
	  $ hiddenWindows 
		$ G.group myMasterGrid
		$ smartSpacingWithEdge 8
    $ multiCol [1] 1 0.02 0.3
myMasterGrid = renamed [ Replace "MGrid" ]
    $ hiddenWindows
    $ mastered (1/100) (10/24) $ GV.Grid (15/13)
myGrid = renamed [ Replace "Grid" ]
    $ hiddenWindows
    $ spacingWithEdge 3
    $ dwmStyle shrinkText mySDConfig
    $ GV.Grid (978/1057)
myFull = smartBorders
    $ Full

mySDConfig = def { 
           inactiveTextColor   = colActive
		 , activeColor         = colActive
		 , inactiveColor       = colGray
		 , urgentColor 	       = colUrgent
		 , activeBorderWidth   = 0
		 , inactiveBorderWidth = 0
		 , decoHeight          = 22
		 , decoWidth           = 190
		 , fontName            = font "9"
		 }

data MAGNIFY = MAGNIFY deriving (Read, Show, Eq)
instance MT.Transformer MAGNIFY Window where
    transform MAGNIFY x k = k ((magnifiercz 1.4) x) (\(ModifiedLayout _ x') -> x')

myModifiers = MT.mkToggle (MIRROR MT.?? FULL MT.?? NOBORDERS MT.?? MAGNIFY MT.?? MT.EOT)

data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ W.floating wset

-- ############################################################################
--                           ACTIONS
-- ############################################################################
showTextConf :: ShowTextConfig
showTextConf = def {
				   st_font = font "23",
				   st_bg   = colInactive,
				   st_fg   = colUrgent
				   }
flashCurrentWin = withFocused 
				  $  \w -> getName w >>= flashText showTextConf 0.35 . show

flashCurrentWS = withWindowSet (pure . W.currentTag) 
                   >>= return . (wrap " " " " ) 
				   >>= flashText showTextConf 1.5 
windowBringerConf :: WindowBringerConfig
windowBringerConf = def { 
    menuArgs = [ "-l", "20"
               , "-i"
               , "-b"
               , "-nb", colInactive
               , "-nf", colFg
               , "-sb", colSep
               , "-sf", colActive
               ],
    windowTitler = \ws -> \w -> do
          name <- show <$> getName w
          className <- show <$> getNameWMClass w
          return $ "  " ++ fixedWidth 5 ( W.tag ws )
                        ++ "   |   " 
                        ++ (fixedWidth 16 className) 
                        ++ "   |   "
                        ++ name
    }
fixedWidth :: Int -> String -> String
fixedWidth 0 s      = ""
fixedWidth x ""     = " " ++ (fixedWidth (x-1) "") 
fixedWidth x (s:sw) = [s] ++ (fixedWidth (x-1) sw)
myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = def { 
                   gs_cellheight = 80
                 , gs_cellwidth = 300
                 , gs_cellpadding = 20
                 , gs_font = font "14"
                 , gs_bordercolor = "snow2"
                 }
xActions = [
           ( "Full", sendMessage $ MT.Toggle FULL)
		 , ( "Mirror", sendMessage $ MT.Toggle MIRROR)
		 , ( "NoBorder", sendMessage $ MT.Toggle NOBORDERS)
		 , ( "Magnify", sendMessage $ MT.Toggle MAGNIFY)
		   ]
spawnPrograms = [
              ( "xterm",        "xterm -bg DeepPink4 -class xterm_gridSelect" )
            , ( "ghci",         "xterm -bg DeepSkyBlue4 -class xterm_gridSelect -e 'stack repl'" )
            ]
spawnSystem = [
      ( "Hibernate", "systemctl hibernate" )
    , ( "Shutdown", "shutdown 0" )
    , ( "Hybrid-Sleep", "systemctl hybrid-sleep" )
    , ( "Journal", "xterm -bg DarkBlue -class xterm_gridSelect -e 'journalctl -r'" )
    , ( "Bildschirm ausschalten", "xset dpms force off" )
    , ( "Suspend", "systemctl suspend" )
    , ( "System Update", "xterm -bg DarkBlue -class xterm_gridSelect -e 'pacman -Qqen > ~/.pkglist_Qqen.txt && pacman -Qqem > ~/.pkglist_Qqem.txt && sudo pacman -Syu'" )
    ]
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

-- ############################################################################
--                           KEYBINDINGS
-- ############################################################################
relevantWorkspaces =  (( Cyc.Not emptyWS ) :&: hiddenWS :&: ignoringWSs ["NSP"] )

myKeys = [ 
      ("M-z",        spawn "slock"                                       )

    , ("M-o",        withFocused (toggleTag "i")  )
    , ("M-i",        focusDownTaggedGlobal "i")

    , ("M-x",        runSelectedAction myGSConfig xActions  )
    , ("M-s",        gridselect        myGSConfig spawnSystem   >>= spawn . fromMaybe "" )
    , ("M-a",        gridselect        myGSConfig spawnPrograms >>= spawn . fromMaybe "" )

    , ("M1-<Tab>",   cycleRecentNonEmptyWS [xK_Alt_L] xK_Tab xK_grave >> flashCurrentWS)
    , ("M-<Tab>",    windows W.focusUp >> flashCurrentWin )
    , ("M-S-<Tab>",  windows W.focusDown >> flashCurrentWin )

    , ("M-m",        windows W.focusMaster                 )
    , ("M-n",        windows W.swapMaster                    )

    , ("M-S-y",      namedScratchpadAction myScratchpads "nnn"        )
    , ("M-y",        namedScratchpadAction myScratchpads "1Password"  )
    , ("M1-y",       namedScratchpadAction myScratchpads "Mixer"  )

    , ("M1-<L>",     Cyc.moveTo Prev relevantWorkspaces >> flashCurrentWS)
    , ("M1-<R>",     Cyc.moveTo Next relevantWorkspaces >> flashCurrentWS)

    , ("M1-h",       Cyc.moveTo Prev relevantWorkspaces >> flashCurrentWS)
    , ("M1-l",       Cyc.moveTo Next relevantWorkspaces >> flashCurrentWS)

    , ("M1-S-<L>",   Cyc.shiftTo Prev relevantWorkspaces >> Cyc.moveTo Prev relevantWorkspaces >> flashCurrentWS)
    , ("M1-S-<R>",   Cyc.shiftTo Next relevantWorkspaces >> Cyc.moveTo Next relevantWorkspaces >> flashCurrentWS)
    , ("M1-S-h",     Cyc.shiftTo Prev relevantWorkspaces >> Cyc.moveTo Prev relevantWorkspaces >> flashCurrentWS)
    , ("M1-S-l",     Cyc.shiftTo Next relevantWorkspaces >> Cyc.moveTo Next relevantWorkspaces >> flashCurrentWS)

    , ("<Page_Up>",  nextWS >> flashCurrentWS)
    , ("<Page_Down>",prevWS >> flashCurrentWS)

    , ("M-j",        N2D.windowGo N2D.D False >> flashCurrentWin      )
    , ("M-k",        N2D.windowGo N2D.U False >> flashCurrentWin                             )
    , ("M-h",        N2D.windowGo N2D.L False >> flashCurrentWin                               )
    , ("M-l",        N2D.windowGo N2D.R False >> flashCurrentWin                               )

    , ("M-S-j",      withFocused hideWindow )
    , ("M-S-k",      popOldestHiddenWindow )

    , ("M-S-h",      N2D.windowSwap N2D.U False >> flashCurrentWin                                )
    , ("M-S-l",      N2D.windowSwap N2D.R False >> flashCurrentWin                          )

    , ("M-<D>",      sendMessage ( Go D )   >> flashCurrentWin                            )
    , ("M-<U>",      sendMessage ( Go U )   >> flashCurrentWin                          )
    , ("M-<L>",      sendMessage ( Go L )   >> flashCurrentWin                          )
    , ("M-<R>",      sendMessage ( Go R )   >> flashCurrentWin                          )
    , ("M-S-<D>",    sendMessage ( Swap D ) >> flashCurrentWin                         )
    , ("M-S-<U>",    sendMessage ( Swap U ) >> flashCurrentWin                           )
    , ("M-S-<L>",    sendMessage ( Swap L ) >> flashCurrentWin                           )
    , ("M-S-<R>",    sendMessage ( Swap R ) >> flashCurrentWin                           )

    , ("M-+",        sendMessage Expand     )
    , ("M--",        sendMessage Shrink     )
    , ("M-S-+",      sendMessage ( G.ToEnclosing $ SomeMessage Expand) )
    , ("M-S--",      sendMessage ( G.ToEnclosing $ SomeMessage Shrink ) )

    , ("<Print>",    unGrab *> spawn "scrot -s $HOME/Regal/Screenshots/%F_%R-screenshot.png"                  )

    , ("M-f",        runOrRaiseMaster "firefox" (className =? "firefox") >> flashCurrentWS )
    , ("M-S-f",      ifWindow (className =? "firefox") (currentWs >>= doShift) idHook)

    , ("M-r",        renameWorkspace def                         )
    , ("M-S-r",      changeDir def                               )

    , ("M-u",        focusUrgent >> flashCurrentWS )

    , ("M-S-g",      bringMenuConfig windowBringerConf)
    , ("M-g",        gotoMenuConfig windowBringerConf >> flashCurrentWS )

    , ("M1-s",      GH.splitGroup )

    , ("M-c",        kill                                        )
    , ("M-<Return>", spawn "xterm" )
    ] 
