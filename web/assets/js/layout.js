// Code in this file is responsible for producing ready for render tree
//
// layout takes declarative definitions of elements
// makes sure that requested constraints are met
// generates ready-to-render data tree, with calculated x's and y's.

import HttpReq from './elements/HttpReq';
import Process from './elements/Process';
import c from './constraint';



export function produceLayout(delta) {
  // first generate elements, where they specified
  // args for coords calculation
  //
  // for now args are just Timestamp and Pids
  // but I want it to be flexible in future
  const reqs = HttpReq.produceElements(delta);
  const procs = Process.produceElements(delta);
  console.log('reqs', reqs);
  console.log('procs', procs);

  // here we should consider all generated elements and their
  // constrains, and produce resolve function
  // that can turn each constraint field into actual numeric value
  const resolve = produceResolveFunc({ HttpReq: reqs, Process: procs });

  // now when we got clear coordinate transforms
  // just generate ready-to-render tree with coords

  return {
    HttpReq: resolveConstraints({ elements: reqs, resolve }),
    Process: resolveConstraints({ elements: procs, resolve }),
  };
}



function resolveConstraints({ elements, resolve }) {
  return elements.map((e) => {
    const { id, key, constraints, Component } = e;
    const result = { id, key, Component };
    for (const ckey in constraints) {
      result[ckey] = resolve(constraints[ckey], e);
    }
    return result;
  });
}



function produceResolveFunc({ HttpReq: reqs, Process: procs }) {
  let timestamps = [];
  let pids = [];

  for (const i in reqs) {
    const cons = reqs[i].constraints;
    for (const key in cons) {
      // walk through all constrains of all elements
      // and extract pids and timestamps
      pids = pids.concat(c.getPids(cons[key]));
      timestamps = timestamps.concat(c.getTimestamps(cons[key]));
    }
  }

  for (const i in procs) {
    const cons = procs[i].constraints;
    for (const key in cons) {
      // walk through all constrains of all elements
      // and extract pids and timestamps
      pids = pids.concat(c.getPids(cons[key]));
      timestamps = timestamps.concat(c.getTimestamps(cons[key]));
    }
  }

  // list of unique values
  const timestamps1 = [...(new Set(timestamps))];
  const pids1 = [...(new Set(pids))];

  timestamps1.sort();

  console.log("timestamps", timestamps1);
  console.log("timestamps", pids1);

  return (constraint, element) => {
    if (constraint.type === 'timestamp') {
      if (constraint.value === 'now') { return timestamps1.length + 1; }
      return timestamps1.indexOf(constraint.value)+1;
    } else if (constraint.type === 'pid') {
      return pids1.indexOf(constraint.value);
    } else {
      throw {
        message: "Unknown constraint type",
        constraint, element
      };
    }
  };
};
