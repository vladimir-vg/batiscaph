// Code in this file is responsible for producing ready for render tree
//
// layout takes declarative definitions of elements
// makes sure that requested constraints are met
// generates ready-to-render data tree, with calculated x's and y's.

import HttpReq from './elements/HttpReq';



export function produceLayout(delta) {
  // first generate elements, where they specified
  // args for coords calculation
  //
  // for now args are just Timestamp and Pids
  // but I want it to be flexible in future
  const reqs = HttpReq.produceElements(delta);

  // here we should consider all generated elements and their
  // constrains, and produce toX, toY functions
  const { toX, toY } = generateCoordFuncs({ HttpReq: reqs });

  // now when we got clear coordinate transforms
  // just generate ready-to-render tree with coords

  return {
    HttpReq: convertToCoords({ elements: reqs, toX, toY })
  };
}



function convertToCoords({ elements, toX, toY }) {
  return elements.map((e) => {
    const { id, key, x1, x2, y1, y2 } = e;
    const result = { id, key };
    if (x1) { result.x1 = toX(x1, e); }
    if (x2) { result.x2 = toX(x2, e); }
    if (y1) { result.y1 = toY(y1, e); }
    if (y2) { result.y2 = toY(y2, e); }
    return result;
  });
}



function generateCoordFuncs({ HttpReq: reqs }) {
  // simply collect and sort all timestamps,
  // count each as one posision on Y
  //
  // same with pids and X coords

  const timestamps = [];
  const pids = [];

  for (const i in reqs) {
    const r = reqs[i];
    if (r.x1 && pids.indexOf(r.x1) == -1) { pids.push(r.x1); }
    if (r.x2 && pids.indexOf(r.x2) == -1) { pids.push(r.x2); }
    if (r.y1 && timestamps.indexOf(r.y1) == -1) { timestamps.push(r.y1); }
    if (r.y2 && timestamps.indexOf(r.y2) == -1) { timestamps.push(r.y2); }
  }

  timestamps.sort();
  pids.sort();

  // create closures that will work using collected timestamps and pids
  const toY = (timestamp, _element) => timestamps.indexOf(timestamp)+1;
  const toX = (pid, _element) => pids.indexOf(pid);

  return { toX, toY };
};
