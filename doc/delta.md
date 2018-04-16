Delta is just a tree that derived from slice of events.
It's possible to merge deltas. Merge of two deltas derived from two consequtive slices of events should result in same tree.
This is required to be able to safely browse slices of events in any direction, always get same picture.

This would require to hold delta producer state along with delta tree for each slice of events,
and ability to merge trees and states of delta producers, which should result in new tree.
TODO: tests for it

For now delta is stored as internal state of the process, for simplicity.
