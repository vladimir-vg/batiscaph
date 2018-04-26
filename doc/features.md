In this file we track list of all available features, their state, how they relate to each other, changelog.
Each feature thas symbolic id like `basic_tracing` or `cowboy_reqs`.
These symbolic names might be useful for referring to exact features, and distinguishing them from others.
It also might be useful to use in testing.



# Basic tracing, `basic_tracing`

Tracing settings that are enabled for any process of interest.
These settings are minimal requirement for process to be displayed on map.

__Current__

For now these events include: spawn, exit, children spawn events.

__Future__

Trace ets, file access calls. Probably aggregated by probe.
Also link/unlink events, proper display of current linking between processes.

__Changelog__

25-04-2018 described current state



# Cowboy requests autotrace, `cowboy_reqs`

Processes that run cowboy handler callbacks are automatically enabled to `basic_tracing`.
Execution time of callbacks is saved and displayed.

__Current__

For now only basic `init` and `handle` callbacks are captured.

__Future__

Trace more handler callbacks. Would be wise to copy official cowboy examples
of different examples and test against them.

Also for now there is no logic for displaying long running requests.

__Changelog__

25-04-2018 described current state



# Process detailed info, `process_info_subcsription`

User see process_info after process selection (pid link click or just on map).
Process info is updated from time to time, if changed.

__Current__

Process info is requested by probe every 3 seconds. Only few particular process_info attrs are fetched.
All sent process_info events are stored, so in future it would allow to observe how it changed.
`basic_tracing` is automatically started when process is selected.

__Future__

Would be great to have UI (also on map) to show how and when process_info changed.
It might be wise to display current `trace_flags`, just to make it more transparent for user.

__Changelog__

26-04-2018 automatic basic_tracing now in Current
25-04-2018 described current state



-----------------------

Not yet implemented features.



# Supervisor tree change display, `supervisor-tree`

Collect and display events about spawn and restarts in supervision tree.

__Future__

Would be great to display it in similar manner as process tree.
Yet it shouldn't be displayed on the process map, because there would be many untraced processes.
These things are slightly different.

Should be possible to scroll back in time and see how supervision tree looked like,
and how it changed. Also should display change in real time. If process restarted, should make it visible.

Need to find a way of proper realtime display of events that happen too quickly (say 10 restarts in second).

__Changelog__

25-04-2018 described current state



# Connected ports, `ports`

Display what ports connected to specific process, display ports.

__Future__

Probably ports should be just displayed on the map, just like processes.
Also this approach might create too much noise and better to find other way.

Also need to think how to display process with many connected ports.
This may require some work on map layout, more fine-grained positioning of elements.

Would be good to see clear display which port is socket and which port is file,
along with other additional info, like path, port number, socket opts.

__Changelog__

25-04-2018 described current state
