Toplevel components that are used in routes should be named like `*Page`.
Should be saved in `pages` dir.

Map elements that should be put in coord layout should be places in `elements` dir.



# Events -> Elements pipeline

 1. Receive delta from server, merge into current delta.
 2. Take current delta, produce preliminary elements. These elements don't have clear coordinates yet.
 3. Take all preliminary elements, run constraint resolver, turn some elements into one (ligatures), generate coord functions
 4. Having coord functions just walk through preliminary elements, produce ready-to-render elements with real coords.
