import System.Exit
import Data.Monoid
import qualified Data.Map        as M

import XMonad

import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as W

import XMonad.Util.SpawnOnce

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, shorten, PP(..))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Spacing (spacingRaw, Border(..))

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

windowCount' :: X (Maybe String)
windowCount' = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X())
keys' conf@(XConfig {modMask = modm}) = M.fromList $
  -- launch terminal
  [ ((modm, xK_Return              ), spawn $ terminal conf)

  -- launch rofi
  , ((modm .|. shiftMask, xK_Return), spawn $ "rofi -show drun")

  -- close focused window
  , ((modm .|. shiftMask, xK_c     ), kill)

  -- Rotate through the available layout algorithms
  , ((modm,               xK_space ), sendMessage NextLayout)

  -- Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size
  , ((modm,               xK_n     ), refresh)

  -- Move focus to the next window
  , ((modm,               xK_Tab   ), windows W.focusDown)

  -- Move focus to the next window
  , ((modm,               xK_j     ), windows W.focusDown)

  -- Move focus to the previous window
  , ((modm,               xK_k     ), windows W.focusUp  )

  -- Move focus to the master window
  , ((modm,               xK_m     ), windows W.focusMaster  )

  -- Swap the focused window and the master window
  , ((modm .|. shiftMask, xK_m     ), windows W.swapMaster)

  -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

  -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

  -- Shrink the master area
  , ((modm,               xK_h     ), sendMessage Shrink)

  -- Expand the master area
  , ((modm,               xK_l     ), sendMessage Expand)

  -- Push window back into tiling
  , ((modm,               xK_t     ), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area
  , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

  -- Deincrement the number of windows in the master area
  , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

  -- Quit xmonad
  , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

  -- Restart xmonad
  , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart" )
  ]
  ++
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

layoutHook' = avoidStruts $ defaultLayout'
  where
    defaultLayout' = tiled ||| Mirror tiled ||| Full
    tiled   = spacingRaw False (Border 0 0 0 0) False (Border 6 6 6 6) True $ Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

manageHook' :: Query (Data.Monoid.Endo WindowSet)
manageHook' = composeOne
  [ transience
  , (className  =? "firefox" <||>
     className  =? "brave-browser") -?> doShift (workspaces' !! 1)
  , className  =? "mpv"             -?> doShift (workspaces' !! 7)
  , title      =? "File transfer*"  -?> doFloat
  , className  =? "Pavucontrol"     -?> doFloat
  , title      =? "sudo"            -?> doFloat
  ] <+> composeAll
  [ manageDocks
  , isFullscreen                    --> doFullFloat
  , isDialog                        --> doCenterFloat
  , role       =? "pop-up"          --> doFloat
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

workspaces' :: [String]
workspaces' = clickable
  $ ["\61729", "\61612", "\61728", "\61686", "\61564", "\61557", "\61441", "\61448", "\xf03e"]
  where
    clickable l = ["%{A1:xdotool set_desktop " ++ show (i :: Int) ++ ":} " ++ ws ++ " %{A}" |
                   (i, ws) <- zip [0 ..] l]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal objectPath interfaceName memberName) {
        D.signalBody = [D.toVariant $ UTF8.decodeString str]
      }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

logHook' :: D.Client -> PP
logHook' dbus = def
  { ppOutput          = dbusOutput dbus
  , ppCurrent         = wrap "%{B#773f3f3f}%{F#fff}%{o#16a085} " " %{B- F- o-}"
  , ppVisible         = wrap "%{F#dd}%{o#666} " " %{F- o-}"
  , ppUrgent          = wrap "%{B#bd2c40}%{F#000}{%o#9b0a20} " " %{B- F- o-}"
  , ppHidden          = wrap "%{F#dd}%{o#666} " " %{F- o-}"
  , ppHiddenNoWindows = wrap "%{F#55} " " %{F-}"
  , ppLayout          = wrap "%{F#B45BCF}" "%{F-}"
  , ppWsSep           = ""
  , ppSep             = "%{F#666666} | %{F-}"
  , ppExtras          = [windowCount']
  , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
  , ppTitle           = shorten 40
  }

startupHook' :: X()
startupHook' = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"
  spawnOnce "$HOME/.config/polybar/launch.sh"
  spawnOnce "clipit"
  spawnOnce "nm-applet"
  spawnOnce "xfce4-power-manager"
  spawnOnce "setcursor"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  spawnOnce "start-pulseaudio-x11"
  spawnOnce "pa-applet"
  setWMName "LG3D"

main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  _ <- D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad $ docks $ ewmh desktopConfig
    { terminal           = "alacritty"
    , modMask            = mod4Mask
    , normalBorderColor  = "#292d3e"
    , focusedBorderColor = "#bbc5ff"
    , startupHook        = startupHook' <+> docksStartupHook
    , workspaces         = workspaces'
    , keys               = keys'
    , layoutHook         = layoutHook'
    , manageHook         = manageHook' <+> manageHook desktopConfig
    , handleEventHook    = fullscreenEventHook <+> handleEventHook desktopConfig <+> docksEventHook
    , logHook            = dynamicLogWithPP (logHook' dbus)
    }
