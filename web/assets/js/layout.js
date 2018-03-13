// Code in this file is responsible for producing ready for render tree
//
// layout takes declarative definitions of *Element's
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
    const { key, x1, x2, y1, y2, attrs } = e;
    return {
      x1: toX(x1, e), x2: toX(x2, e),
      y1: toY(y1, e), y2: toY(y2, e),
      key, attrs,
    }
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
    if (r.x1 && timestamps.indexOf(r.x1) == -1) { timestamps.push(r.x1); }
    if (r.x2 && timestamps.indexOf(r.x2) == -1) { timestamps.push(r.x2); }
    if (r.y1 && pids.indexOf(r.y1) == -1) { pids.push(r.y1); }
    if (r.y2 && pids.indexOf(r.y2) == -1) { pids.push(r.y2); }
  }

  timestamps.sort();
  pids.sort();

  // create closures that will work using collected timestamps and pids
  const toX = (timestamp, _element) => timestamps.indexOf(timestamp);
  const toY = (pid, _element) => pids.indexOf(pid);

  return { toX, toY };
};
