
// This module provides interface to work with constraints.
// Elements use this modules to define dependencies from other elements and values.



export function xPid(pid) {
  return {type: 'pid', value: pid};
}

export function yTimestamp(timestamp) {
  return {type: 'timestamp', value: timestamp};
}



export function getPids(con) {
  if (con.type === 'pid') { return [con.value]; }
  return [];
}

export function getTimestamps(con) {
  if (con.type === 'timestamp') { return [con.value]; }
  return [];
}
