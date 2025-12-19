--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import System.Exit

-- Extensions
import XMonad.Actions.CycleWS
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Actions.GridSelect
import XMonad.Hooks.EwmhDesktops -- REQUIRED for modern fullscreen support
import XMonad.Layout.NoBorders
import XMonad.Layout.IndependentScreens
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "kitty" -- Set to xterm with slightly larger font

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- "windows key" is mod4Mask.
myModMask       = mod4Mask

-- IndependentScreens: each physical screen gets its own set of workspaces
-- This means screen 0 has workspaces 0_1, 0_2, ... 0_9
-- and screen 1 has workspaces 1_1, 1_2, ... 1_9
myWorkspaces    = withScreens 2 ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#cb4b16"

-- Default offset of drawable screen boundaries from each physical
-- screen. Anything non-zero here will leave a gap of that many pixels
-- on the given edge, on the that screen. A useful gap at top of screen
-- for a menu bar (e.g. 15)
-- Fields are: top, bottom, left, right.
myDefaultGaps   = [(0,0,0,0)]

------------------------------------------------------------------------
-- Helper function for IndependentScreens
-- Filters workspaces to only include those on the current physical screen
myScreenWorkspaces :: X (WindowSpace -> Bool)
myScreenWorkspaces = do
    ws <- gets windowset
    -- Get the current physical screen (0, 1, etc.)
    let currentScreen = W.screen (W.current ws)
    -- Filter workspaces that belong to this screen
    return $ \w -> unmarshallS (W.tag w) == fromIntegral currentScreen

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- LAUNCH PROGRAMS
    -- launch a terminal (Alt+Enter default)
    [ ((modMask,                    xK_Return), spawn $ XMonad.terminal conf)
    -- launch Thunar
    , ((modMask .|. shiftMask,      xK_Return), spawn "thunar" )
    -- launch dmenu
    , ((modMask,                    xK_m     ), spawn "dmenu_run -fn 'xft:Monoid:size=10'")
    
    -- MULTIMEDIA KEYS
    , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl --class=backlight set +5%")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl --class=backlight set 5%-")
    , ((0, xF86XK_Display), spawn "~/.local/bin/toggle-color-temperature.sh")
    --, ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    --, ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    --, ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

    -- WORKSPACE
    -- Go to previous workspace (on current screen only)
    , ((modMask,                    xK_d),    moveTo Prev (WSIs myScreenWorkspaces))
    -- Go to next workspace (on current screen only)
    , ((modMask,                    xK_f),    moveTo Next (WSIs myScreenWorkspaces))
    -- Shift to previous workspace (on current screen only)
    , ((modMask .|. shiftMask, xK_d),  shiftTo Prev (WSIs myScreenWorkspaces))
    -- Shift to next workspace (on current screen only)
    , ((modMask .|. shiftMask, xK_f),    shiftTo Next (WSIs myScreenWorkspaces))
      -- close focused window 
    , ((modMask .|. shiftMask,      xK_t     ), kill)
    -- Move focus to the previous window
    , ((modMask,                    xK_o     ), windows W.focusUp  )
    -- Move focus to the next window
    , ((modMask,                    xK_p     ), windows W.focusDown)
    -- Swap the focused window with the previous window
    , ((modMask,                     xK_i    ), windows W.swapUp     )
    -- Shrink the master area
    , ((modMask,                    xK_k     ), sendMessage Shrink)
    -- Expand the master area
    , ((modMask,                    xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modMask,                    xK_j     ), withFocused $ windows . W.sink)
    -- Resize viewed windows to the correct size
    , ((modMask,                    xK_h     ), refresh)
    -- Increment the number of windows in the master area
    , ((modMask                     , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modMask                      , xK_period), sendMessage (IncMasterN (-1)))
    -- Rotate through the available layout algorithms
    , ((mod4Mask,                    xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((mod4Mask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- MULTI-SCREEN SUPPORT
    -- Move window to other screen (keeps window on the active workspace of that screen)
    , ((modMask,                    xK_w     ), shiftNextScreen >> nextScreen)
    -- Switch focus to other screen
    , ((modMask,                    xK_e     ), nextScreen)

    -- Xmonad
    -- Restart xmonad
    , ((modMask .|. shiftMask, xK_z ), restart "xmonad" True)
    -- Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- With IndependentScreens, these operate on the current screen's workspaces
    [((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((mod4Mask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((mod4Mask .|. shiftMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((mod4Mask .|. shiftMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- Other useful ones: Full
myLayout = smartBorders tiled ||| noBorders simpleTabbed ||| Mirror tiled
  where
      -- default tiling algorithm partitions the screen into two panes
      tiled   = Tall nmaster delta ratio

      -- The default number of windows in the master pane
      nmaster = 1

      -- Default proportion of screen occupied by master pane
      ratio = 1/2  
      -- Golden ration:  ratio = toRational (2/(1+sqrt(5)::Double))  

      -- Percent of screen to increment by when resizing panes
      delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"             --> doFloat
    , className =? "Gimp"                --> doFloat
    , className =? "Skype"               --> doFloat
    , className =? "Inkscape"            --> doFloat
    , className =? "feh"                 --> doFloat
    , className =? "Gliv"                --> doFloat
    , className =? "Xchm"                --> doFloat
    , className =? "Wine"                --> doFloat
    , title     =? "opengl"              --> doFloat
    , resource  =? "desktop_window"      --> doIgnore
    , resource  =? "kdesktop"            --> doIgnore ]

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- Monitor setup is handled here to detect and configure DP-2 if connected
myStartupHook = do
    spawn "~/.local/bin/setup-monitors.sh"
    return ()

------------------------------------------------------------------------
-- Run xmonad with the settings you specify. No need to modify this.
-- 
-- The 'ewmh' function is essential for modern fullscreen support (ewmhFullscreen)
-- and proper window management communication with other applications (like status bars).
main = do
    -- Count the number of screens for IndependentScreens
    -- This allows xmonad to adapt when screens are added/removed
    xmonad . ewmh $ defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XConfig.hs
defaults = def {
      -- simple stuff
        terminal                 = myTerminal,
        focusFollowsMouse        = False,
        borderWidth              = myBorderWidth,
        modMask                  = myModMask,
        -- handleEventHook is now handled by the 'ewmh' wrapper in 'main'
        -- handleEventHook          = fullscreenEventHook, 
        -- Changed in 0.10: removed this line
        -- numlockMask            = myNumlockMask,
        workspaces               = myWorkspaces,
        normalBorderColor        = myNormalBorderColor,
        focusedBorderColor       = myFocusedBorderColor,
        --defaultGaps            = myDefaultGaps,

      -- key bindings
        keys                     = myKeys,
        mouseBindings            = myMouseBindings,

      -- hooks, layouts
      -- (showWName for the showname capability -> just remove)
        layoutHook               = showWName myLayout,
        manageHook               = myManageHook,
        logHook                  = myLogHook,
        startupHook              = myStartupHook
    }
