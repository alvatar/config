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
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
-- myTerminal      = "urxvt -is +tr -si -sw +sb -fg '#ffffff' -bg '#111111' -fn 'xft:Andale Mono:pixelsize=28:antialias=true:autohinting=true'"
-- myTerminal      = "urxvtc -sw +sb -fn 'xft:Hack:size=18:antialias=true:autohinting=true'"
myTerminal = "urxvtc"

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
-- Changed in 0.10: removed this line (Alvaro)
-- myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#cb4b16"

-- Default offset of drawable screen boundaries from each physical
-- screen. Anything non-zero here will leave a gap of that many pixels
-- on the given edge, on the that screen. A useful gap at top of screen
-- for a menu bar (e.g. 15)
--
-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(18,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--
myDefaultGaps   = [(0,0,0,0)]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- LAUNCH PROGRAMS
    -- launch a terminal
    [ ((modMask,                xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    -- , ((modMask,               xK_m     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask,               xK_m     ), spawn "dmenu_run -fn 'xft:Monoid:size=10'")
    -- launch gmrun
    --  , ((modMask,               xK_apostrophe     ), spawn "gmrun")
    -- launch internet browser
    -- , ((modMask,               xK_b     ), spawn "opera --notrayicon")
    --, ((modMask,               xK_n     ), spawn "gvim")
    -- launch gvim
    --, ((modMask,               xK_b     ), spawn "emacs")

    -- MULTIMEDIA KEYS
    , ((0, 0x1008ff12), spawn "amixer set Master toggle")
    , ((0, 0x1008ff11), spawn "amixer set Master 5%- && amixer set PCM 5%- -c 1")
    , ((0, 0x1008ff13), spawn "amixer set Master 5%+ && amixer set PCM 5%+ -c 1")
    , ((0, 0x1008ff02), spawn "light -A 5")
    , ((0, 0x1008ff03), spawn "light -U 5")
    -- , ((0, 0x1008ff14), spawn "glxgears")
	
    -- WORKSPACE
    -- Go to previous workspace
    , ((modMask,               xK_d),    prevWS)
    -- Go to next workspace
    , ((modMask,               xK_f),  nextWS)
    -- Shift to next workspace
    , ((modMask .|. shiftMask, xK_d),  shiftToPrev)
    -- Shift to previous workspace
    , ((modMask .|. shiftMask, xK_f),    shiftToNext)
	  -- Choose window from a grid
	  -- , ((mod4Mask, xK_w), goToSelected defaultGSConfig)
    -- close focused window 
    , ((modMask .|. shiftMask,               xK_t     ), kill)
    -- Move focus to the previous window
    , ((modMask,               xK_o     ), windows W.focusUp  )
    -- Move focus to the next window
    , ((modMask,               xK_p     ), windows W.focusDown)
    -- Move focus to the master window
    --  , ((modMask,               xK_p     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    --  , ((modMask,               xK_p), windows W.swapMaster)
    -- Swap the focused window with the next window
    --  , ((modMask,                xK_h     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modMask,                xK_i     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modMask,               xK_k     ), sendMessage Shrink)
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modMask,               xK_j     ), withFocused $ windows . W.sink)
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_h     ), refresh)
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    -- Rotate through the available layout algorithms
    , ((mod4Mask,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((mod4Mask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- toggle the status bar gap
    --, ((modMask              , xK_b     ),
    --      modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i
    --                         in if n == x then (0,0,0,0) else x))

    -- Xmonad
    --, ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modMask .|. shiftMask, xK_z ), restart "xmonad" True)
    -- Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. mod4Mask, k), windows $ f i)
	| (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
	, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    --  ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    --  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  --  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  --  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Skype"          --> doFloat
    , className =? "Inkscape"       --> doFloat
    , className =? "feh"            --> doFloat
    , className =? "Gliv"           --> doFloat
    , className =? "Xchm"           --> doFloat
    , className =? "Wine"           --> doFloat
    , title =? "opengl"  --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

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
myStartupHook = return ()

------------------------------------------------------------------------
-- Run xmonad with the settings you specify. No need to modify this.
main = xmonad defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = False,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        handleEventHook    = fullscreenEventHook,
-- Changed in 0.10: removed this line
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        --defaultGaps        = myDefaultGaps,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
      -- (showWName for the showname capability -> just remove)
        layoutHook         = showWName myLayout,
        manageHook         = myManageHook,
	logHook            = myLogHook,
        startupHook        = myStartupHook
    }
