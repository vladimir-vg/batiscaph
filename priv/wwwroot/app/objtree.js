// In this file contains code related to manipulating objects tree,
// that should be rendered on the map.
//
// Server should return timestamps as single values in microseconds
// Even though resulting integers gonna be large, it should fit into JS
// number type without being rounded (< 2^53).
// Turns out that Neo4j have same limitations for integers (< 2^53).
//
// Rules of layout (not yet implemented):
//
// * Spawned children should be always to the right of parent.
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
  layout.contexts = layout.contexts || {};
  layout.ports = layout.ports || {};

  layout.timestamps = layout.timestamps || [];
  layout.columnsOrder = layout.columnsOrder || [];
  layout.columns = layout.columns || {};

  for (var pid in delta.processes) {
    updateProcessInLayout(delta.processes[pid], layout);
  }

  layout.events = layout.events || [];
  delta.events.forEach(function (event) {
    insertTimestampIntoOrder(event.at, layout);
    layout.events.push(event);
  });
  // layout.events.sort(eventCmpFunction);

  for (var key in delta.contexts) {
    updateContextInLayout(delta.contexts[key], layout);
  }

  for (let key in delta.ports) {
    let port = delta.ports[key];
    insertTimestampIntoOrder(port.openedAt, layout);
    if (port.closedAt) { insertTimestampIntoOrder(port.closedAt, layout); }
    for (let i in port.parts) {
      let part = port.parts[i];
      insertTimestampIntoOrder(part.startedAt, layout);
      if (part.stoppedAt) { insertTimestampIntoOrder(part.stoppedAt, layout); }
    }
    layout.ports[key] = delta.ports[key];
  }

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
  proc.exitReason = data.exitReason;
  proc.disappearedAt = data.disappearedAt;
  proc.registeredName = data.registeredName;
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
    let column = layout.columns[proc.columnId];
    let colproc = column.processes[column.processes.length-1];
    if (colproc.pid != proc.pid) { console.error("last segment in column must be this process", column, proc, colproc); return; }
    colproc.disappearedAt = proc.disappearedAt;
  }

  return null;
};



let updateContextInLayout = (context, layout) => {
  insertTimestampIntoOrder(context.startedAt, layout);
  if (context.stoppedAt)  { insertTimestampIntoOrder(context.stoppedAt, layout); }

  // actually all VARBIND events are expected to be duplicated in process.events
  // so their timestamps should be already inserted into layout

  // just copy as it is
  layout.contexts[context.context] = {
    context: context.context,
    startedAt: context.startedAt,
    stoppedAt: context.stoppedAt,
    pid: context.pid,
    lines: context.lines,
    variables: context.variables,
    evals: context.evals
  };
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

  let tree = {processes: {}, links: {}, spawns: {}, mentions: {}, messages: {}, points: {}, contexts: {}, ports: {}};

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

  let yBiggestPossible = function (at) {
    for (let i in layout.timestamps) {
      i = parseInt(i);
      let at1 = layout.timestamps[i];
      let nextAt = layout.timestamps[i+1];
      if (at > at1 && !nextAt) {
        return i+1;
      } else if (at > at1 && at < nextAt) {
        return i+1;
      } else if (at <= at1) {
        return i;
      }
    }
  }

  let ySmallestPossible = function (at) {
    for (let i in layout.timestamps) {
      i = parseInt(i);
      let at1 = layout.timestamps[i];
      let nextAt = layout.timestamps[i+1];
      if (at > at1 && !nextAt) {
        return i;
      } else if (at > at1 && at < nextAt) {
        return i;
      } else if (at == at1) {
        return i;
      }
    }
  }

  // these mentions will be used to generate mention parts ready to render
  let saveMention = function (at, pid) {
    if (!pid) { console.error("bad pid for saveMention", pid); return; }
    if (!tree.processes[pid]) { tree.processes[pid] = {pid: pid}; }
    if (!tree.processes[pid].mentions) { tree.processes[pid].mentions = []; }
    if (tree.processes[pid].mentions.indexOf(at) != -1) { return; }
    tree.processes[pid].mentions.push(at);
  };

  layout.events.forEach(function (event) {
    let y = yFromTimestamp(event.at);
    let key;
    switch (event.type) {
    case 'SPAWN':
      saveMention(event.at, event.pid1);
      saveMention(event.at, event.pid2);
      key = 'spawn-' + event.at + '-' + event.pid1 + '-' + event.pid2;
      tree.spawns[key] = {y: y, fromX: xFromPid(event.pid1), toX: xFromPid(event.pid2)};
      break;

    case 'LINK':
      saveMention(event.at, event.pid1);
      saveMention(event.at, event.pid2);
      key = 'link-' + event.at + '-' + event.pid1 + '-' + event.pid2;
      tree.links[key] = {y: y, fromX: xFromPid(event.pid1), toX: xFromPid(event.pid2)};
      break;

    case 'VAR_MENTION':
      saveMention(event.at, event.pid1);
      saveMention(event.at, event.pid2);
      tree.contexts[event.context] = tree.contexts[event.context] || {};
      tree.contexts[event.context].mentions = tree.contexts[event.context].mentions || {};
      key = 'var-mention-' + event.at + '-' + event.pid2;
      tree.contexts[event.context].mentions[key] = {y: y, toX: xFromPid(event.pid2), expr: event.expr};
      break;

    case 'MENTION':
      saveMention(event.at, event.pid1);
      saveMention(event.at, event.pid2);
      key = 'mention-' + event.at + '-' + event.pid1 + '-' + event.pid2;
      tree.mentions[key] = {y: y, fromX: xFromPid(event.pid1), toX: xFromPid(event.pid2)};
      break;

    case 'shell_input':
      saveMention(event.at, event.pid);
      key = 'shell-input-' + event.at + '-' + event.pid;
      tree.points[key] = {y: y, x: xFromPid(event.pid)};
      break;

    case 'shell_output':
      // ignore event, if we received it before info about process itself
      if (!(event.pid in layout.processes)) break;

      saveMention(event.at, event.pid);
      key = 'shell-output-' + event.at + '-' + event.pid;
      tree.points[key] = {y: y, x: xFromPid(event.pid)};
      break;
    }
  });

  // take timestamp a bit ahead of last as now
  // will be used for not terminated processes
  let nowY = layout.timestamps.length;

  for (const pid in layout.processes) {
    if (!tree.processes[pid]) { tree.processes[pid] = {mentions: [], pid: pid}; }
    tree.processes[pid].mentions.sort();

    let p = layout.processes[pid];
    tree.processes[pid].x = xFromPid(p.pid);
    tree.processes[pid].startY = yFromTimestamp(p.appearedAt);
    tree.processes[pid].stopY = p.disappearedAt ? yFromTimestamp(p.disappearedAt) : nowY;
    if (p.registeredName) { tree.processes[pid].registeredName = p.registeredName; }

    // now need to iterate over events and over mentions
    // if mention happened during tracing, then ignore it
    // if outside, then make it into visible point
    let produceVisibleParts = function (events, mentions, stopY) {
      let eI = 0, mI = 0;
      let parts = [];
      let traceStartedAt = null;

      let processEvent = function (e) {
        if (e.type == 'TRACE_STARTED') {
          if (traceStartedAt) { console.error("expected to receive TRACE_STARTED once", e, p); return; }
          traceStartedAt = e.at;
        } else if (e.type == 'TRACE_STOPPED') {
          if (!traceStartedAt) { console.error("expected to receive TRACE_STARTED before TRACE_STOPPED", e, p); return; }
          let part = {type: "TRACED", fromY: yFromTimestamp(traceStartedAt), toY: yFromTimestamp(e.at)};
          if (e.at == p.exitedAt) {
            part.exitMark = true;
            part.unnormalExit = (p.exitReason != 'normal')
          }
          if (!p.spawnedAt || (p.spawnedAt != traceStartedAt)) {
            part.untracedHead = true;
          }
          parts.push(part);
          traceStartedAt = null;
        } else if (e.type == 'FOUND_DEAD') {
          parts.push({type: 'DEAD', y: yFromTimestamp(e.at)});
        } else if (e.type == 'BIND') {
          parts.push({type: 'MENTION', y: yFromTimestamp(e.at)});
        }
      }

      let processMention = function (m) {
        if (!traceStartedAt) {
          // we're currently not in trace and encountered mention
          // otherwise just skip this mention
          parts.push({type: 'MENTION', y: yFromTimestamp(m)});
        }
      }

      while (events[eI] || mentions[mI]) {
        let e = events[eI], m = mentions[mI];

        if (e && m) {
          if (m == e.at) {
            // if trace event and mentioned happened at the same time
            // then just ignore that mention
            mI += 1;
          } else if (m < e.at) {
            processMention(m);
            mI += 1;
          } else {
            processEvent(e);
            eI += 1;
          }
        } else if (e) {
          processEvent(e);
          eI += 1;
        } else if (m) {
          processMention(m);
          mI += 1;
        }
      }

      if (traceStartedAt) {
        let part = {type: "TRACED", fromY: yFromTimestamp(traceStartedAt), toY: stopY};
        if (p.exitedAt) {
          part.exitMark = true;
          part.unnormalExit = (p.exitReason != 'normal')
        }
        if (!p.spawnedAt || (p.spawnedAt != traceStartedAt)) {
          part.untracedHead = true;
        }
        parts.push(part);
        traceStartedAt = null;
      }

      return parts;
    }

    tree.processes[pid].parts = produceVisibleParts(p.events, tree.processes[pid].mentions, tree.processes[pid].stopY);
  }


  const lastAt = layout.timestamps[layout.timestamps.length-1];
  for (const key in layout.contexts) {
    let c = layout.contexts[key];
    tree.contexts[key] = tree.contexts[key] || {};
    tree.contexts[key].key = key;
    tree.contexts[key].x = xFromPid(c.pid);
    tree.contexts[key].fromY = yFromTimestamp(c.startedAt);
    if (c.stoppedAt) {
      tree.contexts[key].toY = yFromTimestamp(c.stoppedAt);
    } else {
      tree.contexts[key].toY = yFromTimestamp(lastAt);
      // tree.contexts[key].notTerminated = true;
    }
    tree.contexts[key].lines = c.lines;
    tree.contexts[key].variables = c.variables;
    tree.contexts[key].evals = {};
    for (let line in c.evals) {
      let firstExpr = c.evals[line].exprs[0];
      let lastExpr = c.evals[line].exprs[c.evals[line].exprs.length-1];
      tree.contexts[key].evals[line] = {
        fromY: yBiggestPossible(firstExpr.startedAt),
        toY: yBiggestPossible(lastExpr.stoppedAt) // ySmallestPossible
      };
    }
  }

  for (let key in layout.ports) {
    let port = layout.ports[key];
    tree.ports[key] = {port: port.port, exitReason: port.exitReason, driverName: port.driverName};
    tree.ports[key].parts = [];
    for (let i in port.parts) {
      let part = port.parts[i];
      let treePart = {
        x: xFromPid(part.pid),
        fromY: yFromTimestamp(part.startedAt)
      };
      if (part.stoppedAt) {
        treePart.toY = yFromTimestamp(part.stoppedAt);
      } else {
        treePart.toY = tree.processes[part.pid].stopY;
      }
      tree.ports[key].parts.push(treePart);
    }
  }

  tree.width = layout.columnsOrder.length;
  tree.height = layout.timestamps.length;

  return tree;
};
