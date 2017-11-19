There are several suite files, each containing different tests.

 * __cases_SUITE__ traces parts of erlang code, checks correctness of collected events and produced delta.
   Most of the time you need to write tests there.

 * __delta_SUITE__ tests correct delta generation from specified list of events.
   Sometimes this is needed to check weird combination of events that hard to reproduce by running code.

 * __tree_SUITE__ supposed to check generation JavaScript tree from given delta. Currently not much tests there.
   Should be useful to check that certain combination of events merge into one visual object (like spawn and link right after).
   Also should be useful to check correct layout.

 * __showcases_SUITE__ purpose is to demonstrate different scenarios and how they are drawn on a map.
   No behaviour testing should be done here, only reproducing situations that should demonstrate how complicated information is displayed.
   All rare cases (say process connected 15 different ports) that usually don't happen, but still should be handled somehow.
   Examples in showcases_SUITE are ran by batiscaph_steps, line by line using erl_eval.
   In some rare cases erl_eval runs differently than compiled Erlang code.
