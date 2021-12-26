//! Taken from Keycode.hs
//! https://github.com/kmonad/kmonad/blob/master/src/KMonad/Keyboard/Keycode.hs
use phf::phf_map;

macro_rules! alias_phf_map_helper {
    ( $( $(%$target_platform:expr;)? $( $alias:expr ),+ => $key_value:expr ),+ ) => {
        phf_map! {
            $(
                $(#[cfg(target_os = $target_platform)])?
                $($alias => $key_value),+
            ),+
        }
    }
}

static ALIASES: phf::Map<&'static str, &'static str> = alias_phf_map_helper! {
    "0" => "0",
    "1" => "1",
    "2" => "2",
    "3" => "3",
    "4" => "4",
    "5" => "5",
    "6" => "6",
    "7" => "7",
    "8" => "8",
    "9" => "9",

    "enter", "ret", "return", "ent" => "Enter",
    "minus", "min", "-" => "Minus",
    "equal", "eql", "=" => "Equal",
    "sleep", "zzz" => "Sleep",
    "space", "spc" => "Space",
    "pageup", "pgup" => "PageUp",
    "pagedown", "pgdn" => "PageDown",
    "insert", "ins" => "Insert",
    "delete", "del" => "Delete",
    "volumeup", "volu" => "VolumeUp",
    "volumedown", "voldwn", "vold" => "VolumeDown",
    "brightnessup", "brup", "bru" => "BrightnessUp",
    "brightnessdown", "brdown", "brdwn", "brdn" => "BrightnessDown",
    "leftalt", "lalt", "alt" => "LeftAlt",
    "rightalt", "ralt" => "RightAlt",
    "compose", "comp", "cmps", "cmp" => "Compose",
    "leftshift", "lshift", "lshft", "lsft", "shft", "sft" => "LeftShift",
    "rightshift", "rshift", "rshft", "rsft" => "RightShift",
    "leftctrl", "lctrl", "lctl", "ctl" => "LeftCtrl",
    "rightctrl", "rctrl", "rctl" => "RightCtrl",
    "leftmeta", "lmeta", "lmet", "met" => "LeftMeta",
    "rightmeta", "rmeta", "rmet" => "RightMeta",
    "backspace", "bks", "bspc" => "Backspace",
    "capslock", "caps" => "CapsLock",
    "102nd", "102d", "lsgt", "nubs" => "102nd",
    "forward", "fwd" => "Forward",
    "scrolllock", "scrlck", "slck" => "ScrollLock",
    "print", "prnt" => "Print",
    "wakeup", "wkup" => "WakeUp",
    "left", "lft" => "Left",
    "right", "rght" => "Right",
    "leftbrace", "lbrc", "[" => "LeftBrace",
    "rightbrace", "rbrc", "]" => "RightBrace",
    "semicolon", "scln", ";" => "Semicolon",
    "apostrophe", "apos", "'" => "Apostrophe",
    "grave", "grv", "`" => "Grave",
    "backslash", "bksl", "\\" => "Backslash", // NOTE: "\\" here is a 1char string, the first \ is consumed by Haskell as an escape character
    "comma", "comm", " |" => "Comma",
    "dot", "." => "Dot",
    "slash", "/" => "Slash",
    "numlock", "nlck" => "NumLock",
    "kpslash", "kp/" => "KpSlash",
    "kpenter", "kprt" => "KpEnter",
    "kpplus", "kp+" => "KpPlus",
    "kpasterisk", "kp*" => "KpAsterisk",
    "kpminus", "kp-" => "KpMinus",
    "kpdot", "kp." => "KpDot",
    "sysrq", "ssrq", "sys" => "SysRq",
    "kbdillumdown", "bldn" => "KbdIllumDown",
    "kbdillumup", "blup" => "KbdIllumUp",
    "nextsong", "next" => "NextSong",
    "playpause", "pp" => "PlayPause",
    "previoussong", "prev" => "PreviousSong",

    // purely for casing
    "reserved" => "Reserved",
    "esc" => "Esc",
    "tab" => "Tab",
    "q" => "Q",
    "w" => "W",
    "e" => "E",
    "r" => "R",
    "t" => "T",
    "y" => "Y",
    "u" => "U",
    "i" => "I",
    "o" => "O",
    "p" => "P",
    "a" => "A",
    "s" => "S",
    "d" => "D",
    "f" => "F",
    "g" => "G",
    "h" => "H",
    "j" => "J",
    "k" => "K",
    "l" => "L",
    "z" => "Z",
    "x" => "X",
    "c" => "C",
    "v" => "V",
    "b" => "B",
    "n" => "N",
    "m" => "M",
    "f1" => "F1",
    "f2" => "F2",
    "f3" => "F3",
    "f4" => "F4",
    "f5" => "F5",
    "f6" => "F6",
    "f7" => "F7",
    "f8" => "F8",
    "f9" => "F9",
    "f10" => "F10",
    "kp7" => "Kp7",
    "kp8" => "Kp8",
    "kp9" => "Kp9",
    "kp4" => "Kp4",
    "kp5" => "Kp5",
    "kp6" => "Kp6",
    "kp1" => "Kp1",
    "kp2" => "Kp2",
    "kp3" => "Kp3",
    "kp0" => "Kp0",
    "missing84" => "Missing84",
    "zenkakuhankaku" => "ZenkakuHankaku",
    "f11" => "F11",
    "f12" => "F12",
    "ro" => "Ro",
    "katakana" => "Katakana",
    "hiragana" => "Hiragana",
    "henkan" => "Henkan",
    "katakanahiragana" => "KatakanaHiragana",
    "muhenkan" => "Muhenkan",
    "kpjpcomma" => "Kpjpcomma",
    "linefeed" => "Linefeed",
    "home" => "Home",
    "up" => "Up",
    "end" => "End",
    "down" => "Down",
    "macro" => "Macro",
    "mute" => "Mute",
    "power" => "Power",
    "kpequal" => "KpEqual",
    "kpplusminus" => "KpPlusMinus",
    "pause" => "Pause",
    "scale" => "Scale",
    "kpcomma" => "KpComma",
    "hangeul" => "Hangeul",
    "hanja" => "Hanja",
    "yen" => "Yen",
    "stop" => "Stop",
    "again" => "Again",
    "props" => "Props",
    "undo" => "Undo",
    "front" => "Front",
    "copy" => "Copy",
    "open" => "Open",
    "paste" => "Paste",
    "find" => "Find",
    "cut" => "Cut",
    "help" => "Help",
    "menu" => "Menu",
    "calc" => "Calc",
    "setup" => "Setup",
    "file" => "File",
    "sendfile" => "SendFile",
    "deletefile" => "DeleteFile",
    "xfer" => "Xfer",
    "prog1" => "Prog1",
    "prog2" => "Prog2",
    "www" => "Www",
    "msdos" => "MsDos",
    "coffee" => "Coffee",
    "direction" => "Direction",
    "cyclewindows" => "CycleWindows",
    "mail" => "Mail",
    "bookmarks" => "Bookmarks",
    "computer" => "Computer",
    "back" => "Back",
    "closecd" => "CloseCd",
    "ejectcd" => "EjectCd",
    "ejectclosecd" => "EjectCloseCd",
    "stopcd" => "StopCd",
    "record" => "Record",
    "rewind" => "Rewind",
    "phone" => "Phone",
    "iso" => "Iso",
    "config" => "Config",
    "homepage" => "Homepage",
    "refresh" => "Refresh",
    "exit" => "Exit",
    "move" => "Move",
    "edit" => "Edit",
    "scrollup" => "ScrollUp",
    "scrolldown" => "ScrollDown",
    "kpleftparen" => "KpLeftParen",
    "kprightparen" => "KpRightParen",
    "new" => "New",
    "redo" => "Redo",
    "f13" => "F13",
    "f14" => "F14",
    "f15" => "F15",
    "f16" => "F16",
    "f17" => "F17",
    "f18" => "F18",
    "f19" => "F19",
    "f20" => "F20",
    "f21" => "F21",
    "f22" => "F22",
    "f23" => "F23",
    "f24" => "F24",
    "missing195" => "Missing195",
    "missing196" => "Missing196",
    "missing197" => "Missing197",
    "missing198" => "Missing198",
    "missing199" => "Missing199",
    "playcd" => "PlayCd",
    "pausecd" => "PauseCd",
    "prog3" => "Prog3",
    "prog4" => "Prog4",
    "dashboard" => "Dashboard",
    "suspend" => "Suspend",
    "close" => "Close",
    "play" => "Play",
    "fastforward" => "FastForward",
    "bassboost" => "BassBoost",
    "hp" => "Hp",
    "camera" => "Camera",
    "sound" => "Sound",
    "question" => "Question",
    "email" => "Email",
    "chat" => "Chat",
    "search" => "Search",
    "connect" => "Connect",
    "finance" => "Finance",
    "sport" => "Sport",
    "shop" => "Shop",
    "alterase" => "Alterase",
    "cancel" => "Cancel",
    "media" => "Media",
    "switchvideomode" => "SwitchVideoMode",
    "kbdillumtoggle" => "KbdIllumToggle",
    "send" => "Send",
    "reply" => "Reply",
    "forwardmail" => "ForwardMail",
    "save" => "Save",
    "documents" => "Documents",
    "battery" => "Battery",
    "bluetooth" => "Bluetooth",
    "wlan" => "Wlan",
    "uwb" => "Uwb",
    "unknown" => "Unknown",
    "videonext" => "VideoNext",
    "videoprev" => "VideoPrev",
    "brightnesscycle" => "BrightnessCycle",
    "brightnesszero" => "BrightnessZero",
    "displayoff" => "DisplayOff",
    "wimax" => "Wimax",
    "missing247" => "Missing247",
    "missing248" => "Missing248",
    "missing249" => "Missing249",
    "missing250" => "Missing250",
    "missing251" => "Missing251",
    "missing252" => "Missing252",
    "missing253" => "Missing253",
    "missing254" => "Missing254",
    "missing255" => "Missing255",
    "fn" => "Fn",
    "launchpad" => "Launchpad",
    "missionctrl" => "MissionCtrl"
};

/// Returns Some(normalized keycode) for valid keycodes, `None` otherwise.
/// Alias keycodes are normalized into their standard counterparts.
pub fn normalize_keycode(string: &str) -> Option<&'static str> {
    let lowercased_string = string.to_ascii_lowercase();
    ALIASES.get(&lowercased_string).map(|x| *x)
}
