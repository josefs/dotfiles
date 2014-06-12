import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Graphics.X11.Xlib
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Actions.CycleWS
import System.IO
import System.Exit

main = do
    xmonad =<< statusBar "/home/josef/.cabal/bin/xmobar /home/josef/.xmobarrc"
                         xmobarPP toggleStrutsKey
      (ewmh defaultConfig
        { manageHook      = myManageHooks <+> manageHook defaultConfig
        , terminal        = "gnome-terminal"
        , keys            = myKeys <+> theKeys
        , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
        , layoutHook      = layout
        } `additionalKeys`
        -- Catch the volume buttons.
        -- Source: http://superuser.com/questions/389737/how-do-you-make-volume-keys-and-mute-key-work-in-xmonad
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((0                     , 0x1008FF11), spawn "amixer set Master 1-")
        , ((0                     , 0x1008FF13), spawn "amixer set Master 1+")
        , ((0                     , 0x1008FF12), spawn "amixer -D pulse set Master toggle")
        ])

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
           -- These first two are from the example in the documentation
         [ ((modm, xK_F12), xmonadPrompt defaultXPConfig)
         , ((modm, xK_F3 ), shellPrompt  defaultXPConfig)

           -- Taken from http://debianelectronics.blogspot.se/2012/09/xmonad-screenshot-hotkeys.html
              --take a screenshot of entire display 
         , ((modm , xK_Print), spawn "scrot screen_%Y-%m-%d-%H-%M-%S.png -d 1")

           --take a screenshot of focused window 
         , ((modm .|. controlMask, xK_Print), spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -d 1-u")
           
           -- Rotate through workspaces. Use the keybindings from Ubuntu
         , ((modm .|. controlMask, xK_Right), moveTo Next NonEmptyWS)
         , ((modm .|. controlMask, xK_Left) , moveTo Prev NonEmptyWS)
         , ((modm .|. controlMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
         , ((modm .|. controlMask .|. shiftMask, xK_Left) , shiftToPrev >> prevWS)
         ]

theKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
theKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask .|. controlMask, xK_t   ), spawn $ XMonad.terminal conf) -- %! Same as Ubuntu
    , ((modMask,               xK_p     ), spawn "wmgo") -- %! Launch wmgo
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window
    , ((modMask,               xK_F4    ), kill)

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask .|. shiftMask, xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling
    , ((modMask,               xK_s     ), withFocused $ keysMoveWindow (0,5)) -- %! Move the window horizontally
    , ((modMask,               xK_d     ), withFocused $ keysMoveWindow (5,0))
    , ((modMask .|. shiftMask, xK_s     ), withFocused $ keysMoveWindow (0,-5))
    , ((modMask .|. shiftMask, xK_d     ), withFocused $ keysMoveWindow (-5,0))

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_Escape), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_Escape), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    , ((modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -")) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)
    -- repeat the binding for non-American layout keyboards
    , ((modMask              , xK_question), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    -- Don't use mod-shift-5 because it interfers with Emacs. So no way to go to or more windows to workspace 5
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) (filter (/= xK_5) [xK_1 .. xK_9])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myManageHooks = composeAll
                [ className =? "Loopy"   --> doFloat
                , className =? "Gimp"    --> doFloat
                , className =? "Netgame" --> doFloat
                , className =? "Net"     --> doFloat -- This one doesn't seem to work?!
                , title     =? "gmfrm"   --> doFloat
                ]

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice.
layout = tiled ||| Mirror tiled ||| noBorders Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- I should edit this text, or perhaps throw it out.
-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]-- |

-- Helper function which provides ToggleStruts keybinding
-- Not exported from XMonad.Hooks.DynamicLog
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
