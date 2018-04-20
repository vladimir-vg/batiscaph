
// This module provides interface to work with attributes.
// Elements use this modules to define dependencies from other elements and values.
// In future it may evolve into some constraint logic facts and rules.



function xPid(pid) {
  return {type: 'pid', value: pid};
}

function yTimestamp(timestamp) {
  return {type: 'timestamp', value: timestamp};
}

function yTimestampNow() {
  return {type: 'timestamp', value: 'now'};
}



function getPids(con) {
  if (con.type === 'pid') { return [con.value]; }
  return [];
}

function getTimestamps(con) {
  if (con.type === 'timestamp' && con.value !== 'now') { return [con.value]; }
  return [];
}



export default {
  xPid, yTimestamp, yTimestampNow, getPids, getTimestamps
};