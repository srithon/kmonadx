# 0.4.1

- Fix bug where initial keys defined in the first parent layer could not be
  overriden by future parent layers
  - I'm not sure how I thought this was fixed in v0.3.6; in the following example, `1` was incorrectly bound to `a` in `c`
```kbdx
[a]
[[keys]]
1 = 'a'

[b]
[[keys]]
1 = 'b'

[c]
parent = { a, b }
```

# 0.4.0

- Refactored `diagnostic` module to be more usable as a library

# 0.3.6

- Fix buttons not being able to be overriden more than once in multi-inheritance
    - In the following example, `x` would be incorrectly bound to `y` in `foo`
```kbdx
[a]
[[keys]]
x = 'x'

[b]
[[keys]]
x = 'y'

[c]
[[keys]]
x = 'z'

[foo]
parent = { a, b, c }
```

# 0.3.5

- Allow layer references in constants (`button = (cmd-button "/bin/bash $layer:my-argument")`)

# 0.3.4

- Fix layer reference parsing within Lispy buttons (`(tap-hold 50 @leader:enter XX)`)

# 0.3.3

- Fix bug where single alphabet characters would be automatically capitalized when used as rvalues
    - Outside of the `defsrc` block, KMonad interprets capital letters as being shifted
- Fix finnicky bug where using layer references which pointed to buttons which themselves referred to other aliases within their own layer, would cause compilation to fail ~50% of the time

# 0.3.2

- Fix bugs with tap macro parsing
- Fix single quoted strings not compiling correctly in certain cases

# 0.3.1

- Program now sets the exit code properly instead of printing it out
- Rather than printing the compiled document to stdout, correctly writes it to files
- Program now prints long help when no arguments are passed in

# 0.3.0

- Added support for string/number constant interpolation

Snippet taken from `functional_tutorial.kbdx`
```kbdx
[[private]]
scroll_sh = "~/.config/kmonad/scroll/scroll.sh"
speed_sh = "~/.config/kmonad/scroll/scroll_speed.sh"

speed-up-speed = 50
speed-down-speed = 200

exit = (layer-rem scroll)

# constants can be interpolated within double quoted strings as well
left  = (cmd-button "$scroll_sh h -" "$scroll_sh h 0")
up    = (cmd-button "$scroll_sh v -" "$scroll_sh v 0")
down  = (cmd-button "$scroll_sh v +" "$scroll_sh v 0")
right = (cmd-button "$scroll_sh h +" "$scroll_sh h 0")

speed-up   = (cmd-button "$speed_sh $speed-up-speed"  "$speed_sh 0")
speed-down = (cmd-button "$speed_sh $speed-down-speed" "$speed_sh 0")
```

# 0.2.0

First functional release.
