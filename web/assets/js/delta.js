// Code in this file is responsible for working with deltas
// provided by the serivce
//
// we never remove values from delta, only add
// if value is undefined, we think that it's not computed yet



// this function changes oldDelta in place, return nothing
export function mergeDelta({ oldDelta, newDelta }) {
  mergeObjects(oldDelta, newDelta);
}

function mergeObjects(old, new1) {
  for (const key in new1) {
    assign(key, old, new1);
  }
}

function assign(key, old, new1) {
  if (old[key] === undefined || old[key] === null) {
    old[key] = new1[key];
  } else if (typeof old[key] === 'object' && typeof new1[key] === 'object') {
    // both are objects, need to continue merge recursively
    mergeObjects(old[key], new1[key]);
  } else if (old[key] !== new1[key]) {
    // simply replace old values with new
    old[key] = new1[key];
  }
}
