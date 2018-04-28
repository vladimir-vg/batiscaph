// Code in this file is responsible for working with deltas
// provided by the serivce
//
// we never remove values from delta, only add
// if value is undefined, we think that it's not computed yet

import { isObservableMap } from 'mobx';


// this function changes oldDelta in place, return nothing
export function mergeDelta({ oldDelta, newDelta }) {
  mergeObjects(oldDelta, newDelta);
}

function mergeObjects(old, new1) {
  for (const key in new1) {
    assign(key, old, new1);
  }
}



function get(obj, key) {
  if (obj instanceof Map) {
    return obj.get(key);
  }
  return obj[key];
}

function set(obj, key, value) {
  if (isObservableMap(obj)) {
    obj.set(key, value);
  }
  obj[key] = value;
}



function assign(key, old, new1) {
  if (get(old, key) === undefined || get(old, key) === null) {
    set(old, key, new1[key]);
  } else if (typeof get(old, key) === 'object' && typeof new1[key] === 'object') {
    // both are objects, need to continue merge recursively
    mergeObjects(get(old,key), new1[key]);
  } else if (get(old, key) !== new1[key]) {
    // simply replace old values with new
    set(old, key, new1[key]);
  }
}



export function isTracedAt({ at, pid, delta }) {
  if (!delta['erlang-processes'].has(pid)) { return false; }
  const proc = delta['erlang-processes'].get(pid);

  if (proc.SpawnedAt && proc.SpawnedAt < at) { return true; }
  if (proc.TraceStartedAt && proc.TraceStartedAt < at) { return true; }

  return false;
}
