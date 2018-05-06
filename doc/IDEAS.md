It's better to have achitecture explained from very beginning.
Have a convenient way to modify architecture diagram, make parts of it more detailed.

There is a wonderful online design tool called Figma: https://www.figma.com
They provide [developer API](https://www.figma.com/developers/docs) which allows following:

 * Fetch rendered image of document (PNG, JPEG, SVG).
 * Fetch JSON with all elements, including their absolute coordinates on image,
   names of the elements provided by user.

This allows us to do the following:

 1. Design architecture in any detail we want (several frames) online.
 2. Connect different parts of system with their detailed frames using "Prototype" feature.
    These connections will be present in JSON as "transitionNodeID".
 3. Rename some elements that should link to files in repo as "file:foobar.erl".
 4. Fetch JSON and rendered images (svg is also possible, which is great).
 5. Validate that all "file:foobar.erl" links can be resolved.
 6. Generate html which includes image with clickable areas which lead to more detailed info.

This allows to construct complex diagrams quickly,
change it without pain by anyone who worked with Figma.

It allows to make very quick navigation from very generic design,
right to little details.

Actually it's also very convenient for displaying issues.
Helps to understand what part of the system has most problems.