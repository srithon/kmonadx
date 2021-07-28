//! Taken from Keycode.hs
//! https://github.com/kmonad/kmonad/blob/master/src/KMonad/Keyboard/Keycode.hs
use phf::{phf_map, phf_set};

// The 'Keycode' datatype, as an 'Enum' of all the values a 'Keycode' can take.
static KEY_NAMES: phf::Set<&'static str> = phf_set! {
    "Reserved",
    "Esc",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "0",
    "Minus",
    "Equal",
    "Backspace",
    "Tab",
    "Q",
    "W",
    "E",
    "R",
    "T",
    "Y",
    "U",
    "I",
    "O",
    "P",
    "LeftBrace",
    "RightBrace",
    "Enter",
    "LeftCtrl",
    "A",
    "S",
    "D",
    "F",
    "G",
    "H",
    "J",
    "K",
    "L",
    "Semicolon",
    "Apostrophe",
    "Grave",
    "LeftShift",
    "Backslash",
    "Z",
    "X",
    "C",
    "V",
    "B",
    "N",
    "M",
    "Comma",
    "Dot",
    "Slash",
    "RightShift",
    "KpAsterisk",
    "LeftAlt",
    "Space",
    "CapsLock",
    "F1",
    "F2",
    "F3",
    "F4",
    "F5",
    "F6",
    "F7",
    "F8",
    "F9",
    "F10",
    "NumLock",
    "ScrollLock",
    "Kp7",
    "Kp8",
    "Kp9",
    "KpMinus",
    "Kp4",
    "Kp5",
    "Kp6",
    "KpPlus",
    "Kp1",
    "Kp2",
    "Kp3",
    "Kp0",
    "KpDot",
    "Missing84",
    "ZenkakuHankaku",
    "102nd",
    "F11",
    "F12",
    "Ro",
    "Katakana",
    "Hiragana",
    "Henkan",
    "KatakanaHiragana",
    "Muhenkan",
    "Kpjpcomma",
    "KpEnter",
    "RightCtrl",
    "KpSlash",
    "SysRq",
    "RightAlt",
    "Linefeed",
    "Home",
    "Up",
    "PageUp",
    "Left",
    "Right",
    "End",
    "Down",
    "PageDown",
    "Insert",
    "Delete",
    "Macro",
    "Mute",
    "VolumeDown",
    "VolumeUp",
    "Power",
    "KpEqual",
    "KpPlusMinus",
    "Pause",
    "Scale",
    "KpComma",
    "Hangeul",
    "Hanja",
    "Yen",
    "LeftMeta",
    "RightMeta",
    "Compose",
    "Stop",
    "Again",
    "Props",
    "Undo",
    "Front",
    "Copy",
    "Open",
    "Paste",
    "Find",
    "Cut",
    "Help",
    "Menu",
    "Calc",
    "Setup",
    "Sleep",
    "WakeUp",
    "File",
    "SendFile",
    "DeleteFile",
    "Xfer",
    "Prog1",
    "Prog2",
    "Www",
    "MsDos",
    "Coffee",
    "Direction",
    "CycleWindows",
    "Mail",
    "Bookmarks",
    "Computer",
    "Back",
    "Forward",
    "CloseCd",
    "EjectCd",
    "EjectCloseCd",
    "NextSong",
    "PlayPause",
    "PreviousSong",
    "StopCd",
    "Record",
    "Rewind",
    "Phone",
    "Iso",
    "Config",
    "Homepage",
    "Refresh",
    "Exit",
    "Move",
    "Edit",
    "ScrollUp",
    "ScrollDown",
    "KpLeftParen",
    "KpRightParen",
    "New",
    "Redo",
    "F13",
    "F14",
    "F15",
    "F16",
    "F17",
    "F18",
    "F19",
    "F20",
    "F21",
    "F22",
    "F23",
    "F24",
    "Missing195",
    "Missing196",
    "Missing197",
    "Missing198",
    "Missing199",
    "PlayCd",
    "PauseCd",
    "Prog3",
    "Prog4",
    "Dashboard",
    "Suspend",
    "Close",
    "Play",
    "FastForward",
    "BassBoost",
    "Print",
    "Hp",
    "Camera",
    "Sound",
    "Question",
    "Email",
    "Chat",
    "Search",
    "Connect",
    "Finance",
    "Sport",
    "Shop",
    "Alterase",
    "Cancel",
    "BrightnessDown",
    "BrightnessUp",
    "Media",
    "SwitchVideoMode",
    "KbdIllumToggle",
    "KbdIllumDown",
    "KbdIllumUp",
    "Send",
    "Reply",
    "ForwardMail",
    "Save",
    "Documents",
    "Battery",
    "Bluetooth",
    "Wlan",
    "Uwb",
    "Unknown",
    "VideoNext",
    "VideoPrev",
    "BrightnessCycle",
    "BrightnessZero",
    "DisplayOff",
    "Wimax",
    "Missing247",
    "Missing248",
    "Missing249",
    "Missing250",
    "Missing251",
    "Missing252",
    "Missing253",
    "Missing254",
    "Missing255",

    // TODO: make these MacOS-specific
    "Fn",
    "Launchpad",
    "MissionCtrl"
};

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
    "ret", "return", "ent" => "Enter",
    "min", "-" => "Minus",
    "eql", "=" => "Equal",
    "zzz" => "Sleep",
    "spc" => "Space",
    "pgup" => "PageUp",
    "pgdn" => "PageDown",
    "ins" => "Insert",
    "del" => "Delete",
    "volu" => "VolumeUp",
    "voldwn", "vold" => "VolumeDown",
    "brup", "bru" => "BrightnessUp",
    "brdown", "brdwn", "brdn" => "BrightnessDown",
    "lalt", "alt" => "LeftAlt",
    "ralt" => "RightAlt",
    "comp", "cmps", "cmp" => "Compose",
    "lshift", "lshft", "lsft", "shft", "sft" => "LeftShift",
    "rshift", "rshft", "rsft" => "RightShift",
    "lctrl", "lctl", "ctl" => "LeftCtrl",
    "rctrl", "rctl" => "RightCtrl",
    "lmeta", "lmet", "met" => "LeftMeta",
    "rmeta", "rmet" => "RightMeta",
    "bks", "bspc" => "Backspace",
    "caps" => "CapsLock",
    "102d", "lsgt", "nubs" => "102nd",
    "fwd" => "Forward",
    "scrlck", "slck" => "ScrollLock",
    "prnt" => "Print",
    "wkup" => "WakeUp",
    "lft" => "Left",
    "rght" => "Right",
    "lbrc", "[" => "LeftBrace",
    "rbrc", "]" => "RightBrace",
    "scln", ";" => "Semicolon",
    "apos", "'" => "Apostrophe",
    "grv", "`" => "Grave",
    "bksl", "\\" => "Backslash", // NOTE: "\\" here is a 1char string, the first \ is consumed by Haskell as an escape character
    "comm", " |" => "Comma",
    "." => "Dot",
    "/" => "Slash",
    "nlck" => "NumLock",
    "kp/" => "KpSlash",
    "kprt" => "KpEnter",
    "kp+" => "KpPlus",
    "kp*" => "KpAsterisk",
    "kp-" => "KpMinus",
    "kp." => "KpDot",
    "ssrq", "sys" => "SysRq",
    "bldn" => "KbdIllumDown",
    "blup" => "KbdIllumUp",
    "next" => "NextSong",
    "pp" => "PlayPause",
    "prev" => "PreviousSong"
};

/// Returns Some(normalized keycode) for valid keycodes, `None` otherwise.
/// Alias keycodes are normalized into their standard counterparts.
/// Lowercase letters are normalized into their uppercase counterparts.
pub fn normalize_keycode(string: &str) -> Option<&'static str> {
    macro_rules! check {
        ($string:expr) => {{
            if let Some(string) = KEY_NAMES.get_key($string) {
                Some(string)
            } else if let Some(string) = ALIASES.get($string) {
                Some(string)
            } else {
                None
            }
        }};
    }

    // HACK: puts ascii letters in the english alphabet in uppercase to match KEY_NAMES
    if string.len() == 1 {
        check!(&string.to_ascii_uppercase())
    } else {
        check!(string)
    }
}
