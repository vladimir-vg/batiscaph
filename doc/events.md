# Event type

Every event has `type` field which consists of three parts, for example: `p1 plug:request start`.
First `p1` is some kind of typespec. It indicates that this event bounded only to one pid.

There are typespec for events connected to two pids (`p2 erlang:process link`), or pid and port (`p1r1 erlang:port start`).

Second part of `type` describes to what object it is related (request, process, port, file, etc.)
and in which context.

Last part of the type is a verb, describes what happened.
