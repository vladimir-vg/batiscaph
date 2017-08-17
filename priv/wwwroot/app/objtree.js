// In this file contains code related to manipulating objects tree,
// that should be rendered on the map.
//
// Server should return timestamps as single values in microseconds
// Even though resulting integers gonna be large, it should fit into JS
// number type without being rounded (< 2^53).
// Turns out that Neo4j have same limitations for integers (< 2^53).
//
// Rules of layout:
//
// * (Temporarly dismissed) Spawned children should be always to the right of parent.
// * Mentions of the processes should be located in leftmost column.
//   If process mentioned twice and two mentions create connection,
//   then process should be moved to new right column
//   If mentioned process was expanded, then it also moved into new column
// * Spawn and following link event should collapse into one visible event
// * Several mentions, if they're not connected to certain processes should line up in one Y coordinate.
//   This might be very convenient for initial [whereis(R) || R <- registered()] lookup.
//



// this structure contains columns, processes and events
// it does not contain absolute coords for display, only relative values
// it can be easily manipulated and updated, columns might be reordered
//
// Each column has stack of processes, each having fixed length.
// Position of start of the process is calculated by summing all previous processes lengths.
// Each process contain stack of events, each having coordinate relative to start of the processes.
// It organized in such way that would allow to easily insert some event in past.
//
//
//
// let layout = {
//   processes: {
//     pid: {
//       columnId: id,
//       appearedAt: at,
//       exitedAt: at,
//       exitReason: "normal",
//       disappearedAt: at,
//
//       // only related to appearance of the process,
//       // like TRACE_STARTED, TRACE_STOPPED and MENTION
//       events: []
//     }
//   },
//
//  // other events like SEND, LINK, UNLINK, SPAWN
//  events: [
//    {at: at, type: TYPE, pid1: pid, pid2: pid},
//    ...
//  ]
//
//   timestamps: [at, ...],
//   columnsOrder: [columnId, columnId, ...],
//   columns: {
//     columnId: {
//       id: id,
//       mentionsOnly: false,
//       processes: [
//         {pid, appearedAt, disappearedAt}
//       ]
//     }
//   }
// }



// this function applies new data from server to existing layout
// it should add new columns if needed and reuse old
//
// processes and events in delta are expected to be ordered
// processes must be ordered by first apperance
//
// this function destructively updates layout
V.updateLayout = (delta, layout) => {
  layout.processes = layout.processes || {};
  layout.timestamps = layout.timestamps || [];
  layout.columnsOrder = layout.columnsOrder || [];
  layout.columns = layout.columns || {};

  for (var i in delta.processes) {
    updateProcessInLayout(delta.processes[i], layout);
  }

  layout.events = layout.events || [];
  delta.events.forEach(function (event) {
    insertTimestampIntoOrder(event.at, layout);
    layout.events.push(event);
  });
  layout.events.sort(eventCmpFunction);

  return undefined; // all changes made in place
};

let updateProcessInLayout = (data, layout) => {
  layout.processes[data.pid] = layout.processes[data.pid] || {pid: data.pid, events: []};
  let proc = layout.processes[data.pid];

  let wasOpen = !!proc.appearedAt;
  let wasClosed = !!proc.disappearedAt;

  // these properties should return up to date with every delta
  // so simply update them
  proc.appearedAt = data.appearedAt;
  proc.spawnedAt = data.spawnedAt;
  proc.exitedAt = data.exitedAt;
  proc.exitedReason = data.exitedReason;
  proc.disappearedAt = data.disappearedAt;
  if (proc.appearedAt) { insertTimestampIntoOrder(proc.appearedAt, layout); }
  if (proc.spawnedAt) { insertTimestampIntoOrder(proc.spawnedAt, layout); }
  if (proc.exitedAt) { insertTimestampIntoOrder(proc.exitedAt, layout); }
  if (proc.disappearedAt) { insertTimestampIntoOrder(proc.disappearedAt, layout); }

  data.events.forEach(function (event) {
    insertTimestampIntoOrder(event.at, layout);
    proc.events.push(event);
  });

  proc.events.sort(eventCmpFunction);

  if (!wasOpen) {
    if (proc.columnId) { console.error("proc not supposed to have columnId, was just opened", proc); return; }
    let columnId = selectColumnAvailableAt(proc.appearedAt, layout);
    proc.columnId = columnId;
    layout.columns[columnId].processes.push({pid: proc.pid, appearedAt: proc.appearedAt, disappearedAt: proc.disappearedAt});
  } if (wasOpen && !wasClosed) {
    if (!proc.columnId) { console.error("proc supposed to have columnId, was already opened before", proc); return; }
    let column = layout.columns[oldProc.columnId];
    let colproc = column.processes[column.processes.length-1];
    if (colproc.pid != proc.pid) { console.error("last segment in column must be this process", column, proc, colproc); return; }
    colproc.disappearedAt = proc.disappearedAt;
  }

  return null;
};



let eventCmpFunction = function (a, b) {
  if (a.at < b.at) { return -1; }
  if (a.at > b.at) { return 1; }
  return 0;
};



let insertTimestampIntoOrder = (at, layout) => {
  if (!at) { console.error("got bad value to insert: ", at); return; }

  // slow, but simple
  if (layout.timestamps.indexOf(at) == -1) {
    layout.timestamps.push(at);
    layout.timestamps.sort();
  }
};

let selectColumnAvailableAt = (at, layout) => {
  if (!at) { console.error("got bad value for column select: ", at); return; }

  for (let i in layout.columnsOrder) {
    let id = layout.columnsOrder[i];
    let lastProc = layout.columns[id].processes[layout.columns[id].processes.length-1];
    if (!lastProc) {
      console.error("Unexpected column without processes: ", id, layout.columns[id]);
      return;
    }
    if (lastProc.disappearedAt && lastProc.disappearedAt < at) {
      return id;
    }
  }

  // no suitable column found, add fresh one:
  let columnId = Math.random().toString(36).substr(2, 7);
  layout.columns[columnId] = {
    id: columnId,
    processes: []
  };
  layout.columnsOrder.push(columnId);
  return columnId;
};



// this function walks layout and produces objtree with objects
// later from-to options should be added, for rendering big set of objects
// all events in layount expected to be sorted
//
// should produce ready to render tree, no additional check should be necessary, plain mapping to pixels
V.produceTree = (layout) => {
  // some events could be collapsed into one (like SPAWN,LINK pair)
  // to achieve that we need to collect mapping that T1,T2 translate into one Y coord
  // so events should be processed first
  // only then processes
  // TODO: implemented collapsing

  let tree = {processes: {}, links: {}, spawns: {}, messages: {}};

  let xFromPid = function (pid) {
    let columnId = layout.processes[pid].columnId;
    let x = layout.columnsOrder.indexOf(columnId);
    if (x == -1) { console.error("for -1 for columnId check", columnId, layout.processes[pid], layout.columnsOrder); return; }
    return x;
  }

  let yFromTimestamp = function (at) {
    let y = layout.timestamps.indexOf(at);
    if (y == -1) { console.error("got -1 for event.at timestamp search", event); debugger; return; }
    return y;
  }

  layout.events.forEach(function (event) {
    let y = yFromTimestamp(event.at);
    let key;
    switch (event.type) {
    case 'SPAWN':
      key = 'spawn-' + event.at + '-' + event.pid1 + '-' + event.pid2;
      tree.spawns[key] = {y: y, fromX: xFromPid(event.pid1), toX: xFromPid(event.pid2)};
      break;
    case 'LINK':
      key = 'link-' + event.at + '-' + event.pid1 + '-' + event.pid2;
      tree.links[key] = {y: y, fromX: xFromPid(event.pid1), toX: xFromPid(event.pid2)};
      break;
    }
  });

  // take timestamp a bit ahead of last as now
  // will be used for not terminated processes
  let nowY = layout.timestamps.length + 1;

  for (const pid in layout.processes) {
    let p = layout.processes[pid];
    let x = xFromPid(p.pid);
    let startY = yFromTimestamp(p.appearedAt);
    let stopY = p.disappearedAt ? yFromTimestamp(p.disappearedAt) : nowY;
    let parts = [];

    let traceStartedAt = null;
    p.events.forEach(function (event) {
      if (event.type == 'TRACE_STARTED') {
        traceStartedAt = event.at;
      } else if (event.type == 'TRACE_STOPPED') {
        if (!traceStartedAt) { console.error("expected to receive TRACE_STARTED before TRACE_STOPPED", event, p); return; }
        parts.push({type: "TRACED", fromY: yFromTimestamp(traceStartedAt), toY: yFromTimestamp(event.at)});
        traceStartedAt = null;
      }
    });

    if (traceStartedAt) {
      parts.push({type: "TRACED", fromY: yFromTimestamp(traceStartedAt), toY: stopY});
      traceStartedAt = null;
    }
    tree.processes[pid] = {x: x, startY: startY, stopY: stopY, parts: parts};
  }

  return tree;
};



// this structure is produced from layout described above for given coord range
// it contains all objects that should be rendered and all absolute coordinates
// it does not contain any pixel coordinates, only absolute layout coordinates
//
// this structure should be convenient for quick render
// it not supposed to be manipulated or changed
// let objtree = {
//   processes: {
//     pid: {
//       x: N,
//       startY: N, // appearedAt, disappearedAt
//       stopY: N,  // don't really need for display, only for debug
//       parts: [
//         {type: 'MENTIONED', y: N},
//         {type: 'TRACED', fromY: N, toY: N},
//       ]
//     }
//   },
//
//   messages: {
//     key: {y: N, fromX: N, toX: N}
//   },
//   spawns: {
//     key: {y: N, fromX: N, toX: N}
//   }
//   links: {
//     key: {y: N, fromX: N, toX: N}
//   }
// };
