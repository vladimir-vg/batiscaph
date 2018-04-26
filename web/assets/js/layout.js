// Code in this file is responsible for producing ready for render tree
//
// layout takes declarative definitions of elements
// makes sure that requested constraints are met
// generates ready-to-render data tree, with calculated x's and y's.

import HttpReq from './elements/HttpReq';
import Process from './elements/Process';
import attr from './attr';



export function produceLayout(delta) {
  // first generate elements, where they specified
  // args for coords calculation
  //
  // for now args are just Timestamp and Pids
  // but I want it to be flexible in future
  const reqs = HttpReq.produceElements(delta);
  const procs = Process.produceElements(delta);
  // console.log('reqs', reqs);
  // console.log('procs', procs);

  // here we should consider all generated elements and their
  // constrains, and produce resolve function
  // that can turn each constraint field into actual numeric value
  const resolve = produceResolveFunc(delta, { HttpReq: reqs, Process: procs });

  // now when we got clear coordinate transforms
  // just generate ready-to-render tree with coords

  return {
    HttpReq: resolveAttrs({ elements: reqs, resolve }),
    Process: resolveAttrs({ elements: procs, resolve }),
    xColsLength: resolve('xColsLength'),
    yRowsLength: resolve('yRowsLength'),
  };
}



function resolveAttrs({ elements, resolve }) {
  return elements.map((e) => {
    const { id, key, attrs, Component } = e;
    const result = { id, key, Component };
    for (const ckey in attrs) {
      result[ckey] = resolve(attrs[ckey], e);
    }
    return result;
  });
}



function produceResolveFunc(delta, { HttpReq: reqs, Process: procs }) {
  let timestamps = [];
  let pids = [];

  for (const i in reqs) {
    const cons = reqs[i].attrs;
    for (const key in cons) {
      // walk through all constrains of all elements
      // and extract pids and timestamps
      pids = pids.concat(attr.getPids(cons[key]));
      timestamps = timestamps.concat(attr.getTimestamps(cons[key]));
    }
  }

  for (const i in procs) {
    const cons = procs[i].attrs;
    for (const key in cons) {
      // walk through all constrains of all elements
      // and extract pids and timestamps
      pids = pids.concat(attr.getPids(cons[key]));
      timestamps = timestamps.concat(attr.getTimestamps(cons[key]));
    }
  }

  // list of unique values
  const timestamps1 = [...(new Set(timestamps))];
  const pids1 = [...(new Set(pids))];

  timestamps1.sort();

  // to ensure that all spawns happen from left to right
  // create a tree of spawns, and simply walk it
  // poping out each node as column
  let spawnTree = {};
  for (const i in pids1) {
    const proc = delta['erlang-processes'][pids1[i]];
    ensureSavedInSpawnTree(spawnTree, delta['erlang-processes'], proc);
  }
  const pids2 = enumerateSpawnTree(spawnTree);
  spawnTree = null; // not needed anymore, free memory

  // console.log("timestamps", timestamps1);
  // console.log("pids", pids2);

  return (attr1, _element) => {
    if (attr1 === 'xColsLength') { return pids2.length; }
    if (attr1 === 'yRowsLength') { return timestamps1.length; }

    if (attr1.type === 'timestamp') {
      if (attr1.value === 'now') { return timestamps1.length + 1; }
      return timestamps1.indexOf(attr1.value)+1;
    } else if (attr1.type === 'pid') {
      return pids2.indexOf(attr1.value);
    } else {
      return attr1;
    }
  };
};



function ensureSavedInSpawnTree(tree, procs, proc) {
  // TODO: might be sped up by using table of previously calculated trees for pids

  if (!proc.ParentPid || !procs[proc.ParentPid]) {
    if (!tree[proc.Pid]) { tree[proc.Pid] = {}; }
    return tree[proc.Pid];
  }

  const parentTree = ensureSavedInSpawnTree(tree, procs, procs[proc.ParentPid]);
  if (!parentTree[proc.Pid]) { parentTree[proc.Pid] = {}; }
  return parentTree[proc.Pid];
}



function enumerateSpawnTree(tree, stack) {
  if (!stack) { stack = []; }
  for (const pid in tree) {
    stack.push(pid);
    enumerateSpawnTree(tree[pid], stack);
  }
  return stack;
}
