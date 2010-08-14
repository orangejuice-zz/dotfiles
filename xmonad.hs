-- Imports
-- {{{
{-# LANGUAGE NoMonomorphismRestriction #-}

import XMonad
import XMonad.Operations

import XMonad.Actions.DwmPromote
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.Search
import XMonad.Actions.WindowNavigation
import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.WindowBringer

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints ( layoutHints )
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import qualified XMonad.Layout.HintedGrid as HG
import XMonad.Layout.CenteredMaster
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spiral

import XMonad.Prompt            
import XMonad.Prompt.Shell       
import XMonad.Prompt.Ssh
import XMonad.Prompt.Man
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Workspace

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Themes
import XMonad.Util.Scratchpad
import XMonad.Util.NamedWindows

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import Control.Monad (ap, (<=<))

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import Data.Bits ((.|.))
import Data.Ratio
import Graphics.X11                                                                                                    
import System.IO
import System.Posix.Unistd

-- }}}

-- Main configuration
-- {{{
main = do 
    xmonad =<< myConfig

myConfig = do
    xmproc <- spawnPipe "xmobar"
    host <- getHost
    checkTopicConfig myTopics myTopicConfig
    withWindowNavigation ( xK_s, xK_h, xK_x, xK_l ) $ defaultConfig
        { borderWidth        = 1 
        , normalBorderColor  = blackColor
        , focusedBorderColor = greenColor
        , workspaces         = myTopics
        , terminal           = myTerminal
        , modMask            = mod4Mask
        , focusFollowsMouse  = False
        , manageHook         = myManageHook <+> scratchpadManageHookDefault
        , keys               = \c -> myKeys host c `M.union` (foldr M.delete (keys defaultConfig c) (myDeletedKeys c))
        , logHook            = dynamicLogWithPP $ xmobarPP
                                    { ppCurrent = xmobarColor yellowColor "" . wrap "[" "]" 
                                    , ppLayout  = xmobarColor blueColor "" . 
                                         ( \x -> case x of
                                                "Hinted ResizableTall" -> "[^]"
                                                "Tabbed Simplest"      -> "[T]"
                                                "Grid"                 -> "[+]"
                                                "Hinted ThreeCol"      -> "[3]"
                                                "Grid False"           -> "[+]"
                                                "Full"                 -> "[ ]"
                                                _                      -> x
                                         )
                                    , ppOutput  = hPutStrLn xmproc
                                    , ppTitle   = (\x -> "")
                                    }
        , layoutHook        = avoidStruts
                            $ onWorkspace "dev" rtile 
                            $ onWorkspace "misc-term" rtile
                            $ onWorkspace "irc" (smartBorders Full) 
                            $ onWorkspace "documents" rtile
                            $ onWorkspace "web" (smartBorders Full)
                            $ onWorkspace "music" (smartBorders Full)
                            $ onWorkspace "movies" (smartBorders Full)
                            $ onWorkspace "pdf" tabThat
                            $ onWorkspace "cheatsheets" tabThat
                            $ rtile ||| HG.Grid False ||| tabThat ||| gridMaster
        } 
       
-- }}}

-- Host determination
-- {{{

data Host = Whitehat | Pinkhat
    deriving (Eq, Show, Read)

getHost :: IO Host
getHost = do
    hostName <- nodeName `fmap` getSystemID
    return $ case hostName of
        "Whitehat" -> Whitehat
        "Pinkhat"  -> Pinkhat    
        _          -> Whitehat

-- }}}

-- Colors
-- {{{
backGroundColor = "#1C1C1C"
foreGroundColor = "#FFEEFF"
greenColor      = "#B7CE42"
blueColor       = "#66AABB"
yellowColor     = "#FEA63C"
blackColor      = "#050505"
-- }}}

-- Layouts
-- {{{

rtile      = layoutHints ( ResizableTall 1 (3%100) (1/2) [] )
threeSome  = layoutHints ( ThreeCol 1 (3%100) (1/2) ) -- lolwut
tabThat    = tabbedAlways shrinkText myTheme -- tap that??
gridMaster = centerMaster Grid

-- }}}

-- Topics
-- {{{
myTopics :: [Topic]
myTopics =
   [ "dev"
   , "irc"
   , "documents"
   , "terms"
   , "web"
   , "movies"
   , "music"
   , "office"
   , "picard"
   , "pdf"
   , "cheatsheets"
   , "other"
   ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("movies"         , "/media/blackhat"         )
        , ("dev"            , "~/src"                   )
        , ("music"          , "/media/blackhat/music"   )
        , ("terms"          , "~/"                      )
        , ("documents"      , "~/doc"                   )
        , ("pdf"            , "~/doc/paper"             )
        , ("cheatsheets"    , "~/doc/cheatsheets"       )
        , ("office"         , "~/doc"                   )
        ]
    , defaultTopicAction = const $ spawnShell >*> 3
    , defaultTopic = "web"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
        [ ("cheatsheets", spawn "geeqie -t ~/doc/cheatsheets/vim-cheat-sheet-1.png"
                       >> spawn "geeqie -t ~/doc/cheatsheets/vim-cheat-sheet-2.png"
                       >> spawn "geeqie -t ~/doc/cheatsheets/git-cheat-sheet-medium.png" )
        , ("irc",         ssh "blackhat.endoftheinternet.org"                            )
        , ("music",       spawnInShell "ncmpcpp"                                         )
        , ("web",         spawn myBrowser                                                )
        , ("movies",      spawnShellIn "/media/blackhat"                                 )
        , ("dev",         spawnShellIn "~/src" >*> 3                                     )
        , ("documents",   spawnShellIn "~/doc" >*> 2                                     )
        , ("pdf",         spawn "okular"                                                 ) 
        , ("office",      spawn "soffice"                                                )
        , ("picard",      spawn "picard"                                                 )
        ]
    }
-- }}}

-- Keys
-- {{{
myDeletedKeys x =
    [ (modMask x,             xK_h)
    , (modMask x,             xK_l)  
    , (modMask x,             xK_s) ]

myKeys host conf@(XConfig { XMonad.modMask = modMask }) = M.fromList $
    [ ((modMask,               xK_Return ), dwmpromote                                         )
    , ((modMask,               xK_a      ), sendMessage MirrorExpand                           )
    , ((modMask,               xK_z      ), sendMessage MirrorShrink                           )
    , ((modMask,               xK_Delete ), kill                                               )
    , ((modMask,               xK_0      ), toggleWS                                           )
    , ((modMask,               xK_minus  ), sendMessage Shrink                                 )
    , ((modMask,               xK_equal  ), sendMessage Expand                                 )
    , ((modMask              , xK_g      ), promptedGoto                                       )
    , ((modMask .|. shiftMask, xK_g      ), promptedShift                                      )
    , ((modMask              , xK_f      ), goToSelected myLargeGsConfig                       )
    , ((modMask              , xK_w      ), goto "web"                                         )
    , ((modMask              , xK_i      ), goto "irc"                                         )
    , ((modMask              , xK_d      ), goto "dev"                                         )
    , ((modMask              , xK_t      ), goto "terms"                                       )
    , ((modMask              , xK_r      ), currentTopicAction myTopicConfig                   )
    , ((modMask,               xK_F1     ), shellPrompt promptConfig                           )
    , ((modMask,               xK_F2     ), sshPrompt promptConfig                             )
    , ((modMask,               xK_F3     ), manPrompt promptConfig                             )
    , ((modMask,               xK_n      ), appendFilePrompt promptConfig "~/doc/notes/notes"  )

    , ((modMask,               xK_slash  ), submap . M.fromList $ 
        [ ((0, xK_g     ), promptSearchBrowser promptConfig myBrowser google    >> viewWeb )
        , ((0, xK_w     ), promptSearchBrowser promptConfig myBrowser wikipedia >> viewWeb )
        , ((0, xK_c     ), promptSearchBrowser promptConfig myBrowser cplusplus >> viewWeb )
        , ((0, xK_b     ), promptSearchBrowser promptConfig myBrowser binsearch >> viewWeb )
        , ((0, xK_a     ), promptSearchBrowser promptConfig myBrowser archwiki  >> viewWeb )
        , ((0, xK_m     ), promptSearchBrowser promptConfig myBrowser msdn      >> viewWeb )
        , ((0, xK_h     ), promptSearchBrowser promptConfig myBrowser hoogle    >> viewWeb )
        ] )

   , ((modMask .|. shiftMask, xK_slash   ), submap . M.fromList $
        [ ((0, xK_g     ), selectSearch google                 >> viewWeb )
        , ((0, xK_w     ), selectSearch wikipedia              >> viewWeb )
        ] )

    , ((modMask,               xK_Down   ), case host of 
                                                Whitehat -> spawn "amixer -c 0 sset Master 2dB-"
                                                Pinkhat  -> spawn "ossmix vmix0-outvol -- -1" 
                                                _        -> spawn "" )
    , ((modMask,               xK_Up     ), case host of
                                                Whitehat -> spawn "amixer -c 0 sset Master 2dB+"
                                                Pinkhat  -> spawn "ossmix vmix0-outvol -- +1" 
                                                _        -> spawn "" )
                                                
    , ((modMask,               xK_F5     ), case host of 
                                                Pinkhat  -> spawn "smartdimmer -d"
                                                _        -> spawn "" )
    , ((modMask .|. shiftMask, xK_F5     ), case host of 
                                                Pinkhat  -> spawn "smartdimmer -s 1"
                                                _        -> spawn "" )
    , ((modMask,               xK_F6     ), case host of
                                                Pinkhat  -> spawn "smartdimmer -i"
                                                _        -> spawn "" )
    , ((modMask .|. shiftMask, xK_F6     ), case host of
                                                Pinkhat  -> spawn "smartdimmer -s 21"
                                                _        -> spawn "" )
    
    , ((modMask,               xK_m), submap . M.fromList $
        [ ((0, xK_s     ), spawn "mpc stop"   )
        , ((0, xK_p     ), spawn "mpc toggle" )
        , ((0, xK_Right ), spawn "mpc next"   )
        , ((0, xK_Left  ), spawn "mpc prev"   )
        , ((0, xK_r     ), spawn "mpc repeat" )
        , ((0, xK_z     ), spawn "mpc random" )
        , ((0, xK_l     ), spawn "msearch"    )
        , ((0, xK_n     ), spawn "nowplaying" )
        , ((0, xK_m     ), goto "music"       )
       ]
     )
    ]
-- }}}

-- Websearches, workspace switcher 
-- {{{
viewWeb   = goto "web"
cplusplus = searchEngine "cplusplus" "http://www.cplusplus.com/query/search.cgi?q="
binsearch = searchEngine "binsearch" "http://www.binsearch.net/?q="
archwiki  = searchEngine "archwiki"  "http://wiki.archlinux.org/index.php/?search="
msdn      = searchEngine "msdn"      "http://social.msdn.microsoft.com/Search/en-US/?query="

workspaceGridSelect = gridselect mySmallGsConfig <=< asks $ map (\x -> (x,x)) . workspaces . config
-- }}}

-- Theme configuration
-- {{{
myTheme = defaultTheme { inactiveBorderColor = blackColor
                       , activeColor         = backGroundColor
                       , inactiveColor       = backGroundColor
                       , activeBorderColor   = greenColor
                       , activeTextColor     = blueColor
                       , inactiveTextColor   = greenColor
                       , decoHeight          = 15 
                       , decoWidth           = 20
                       , urgentColor         = blackColor
                       , urgentTextColor     = greenColor }

promptConfig = defaultXPConfig { font        = "-*-proggyoptis-medium-r-normal-*-10-*-*-*-*-*-*-*"
                               , fgColor     = foreGroundColor
                               , bgColor     = backGroundColor
                               , bgHLight    = backGroundColor
                               , fgHLight    = blueColor
                               , borderColor = greenColor }

mySmallGsConfig = defaultGSConfig { gs_cellheight = 50
                                  , gs_cellwidth  = 100 }

myLargeGsConfig = defaultGSConfig { gs_cellheight = 50
                                  , gs_cellwidth  = 300 }

-- }}}

-- Managehook
-- {{{
myManageHook = composeAll
    [ className =? "MPlayer"                --> doFloat
    , className =? "Gimp"                   --> doFloat 
    , className =? "Gpicview"               --> doFloat 
    , className =? "XClock"                 --> doFloat
    , className =? "vlc"                    --> doFloat
    , className =? "Shiretoko"              --> doF (W.shift "web") 
    , className =? "vimprobable"            --> doF (W.shift "web") 
    , className =? "uzbl"                   --> doF (W.shift "web") 
    , className =? "OpenOffice.org 3.1"     --> doF (W.shift "office") 
    , className =? "Lyx"                    --> doF (W.shift "lyx") 
    , className =? "Xpdf"                   --> doF (W.shift "pdf") 
    , className =? "Okular"                 --> doF (W.shift "pdf")
    , title     =? "VLC (XVideo output)"    --> doFloat ]
-- }}}

-- Vars
-- {{{
myBrowser  = "firefox"
myTerminal = "urxvtc"
-- }}}

-- Utility functions
-- {{{

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: String -> X ()
spawnShellIn dir = spawn $ "urxvtc -cd " ++ dir

ssh :: String -> X ()
ssh host = spawnInShell ("ssh " ++ host ++ " ; exit 0")

spawnInShell :: String -> X ()
spawnInShell cmd = spawn $ "urxvtc --hold -e " ++ cmd

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt promptConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt promptConfig $ windows . W.shift

--}}}