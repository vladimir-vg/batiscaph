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
//       disappearedAt: at,
//       events: []
//     }
//   },
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
  return undefined; // all changes made in place
};

let updateProcessInLayout = (proc, layout) => {
  if (proc.pid in layout.processes) {
    let oldProc = layout.processes[proc.pid];
    if (proc.appearedAt && oldProc.appearedAt != proc.appearedAt) {
      console.error("process from delta have different apperance time", proc, oldProc);
      return;
    }

    // process stopped
    if (proc.disappearedAt && !oldProc.disappearedAt) {
      oldProc.disappearedAt = proc.disappearedAt;
      insertTimestampIntoOrder(proc.disappearedAt, layout);

      let column = layout.columns[oldProc.columnId];
      let colproc = column.processes[column.processes.length-1];
      if (colproc.pid != proc.pid) {
        console.error("expected last process in column to be the updating one", proc, oldProc, column);
        return;
      }

      colproc.disappearedAt = proc.disappearedAt;
    }
  } else {
    let columnId = selectColumnAvailableAt(proc.appearedAt, layout);
    layout.processes[proc.pid] = {
      appearedAt: proc.appearedAt,
      columnId: columnId,
      events: []
    };
    let colproc = {pid: proc.pid, appearedAt: proc.appearedAt};
    insertTimestampIntoOrder(proc.appearedAt, layout);
    if (proc.disappearedAt) {
      colproc.disappearedAt = proc.disappearedAt;
      insertTimestampIntoOrder(proc.disappearedAt, layout);
      layout.columns[columnId].processes.push(colproc);
    } else {
      layout.columns[columnId].processes.push(colproc);
    }
  }
};

let insertTimestampIntoOrder = (at, layout) => {
  if (!at) {
    console.error("got bad value to insert: ", at);
    return;
  }

  // slow, but simple
  if (layout.timestamps.indexOf(at) == -1) {
    layout.timestamps.push(at);
    layout.timestamps.sort();
  }
};

let selectColumnAvailableAt = (at, layout) => {
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


// let updateProcessInLayout = (proc, layout) => {
//   if (proc.pid in layout.processes) {
//     let oldProc = layout.processes[proc.pid];
//     if (oldProc.appearedAt != proc.appearedAt) {
//       console.error("process from delta have different apperance time", proc, oldProc);
//       return;
//     }
// 
//     // process stopped
//     if (proc.disappearedAt && !oldProc.disappearedAt) {
//       oldProc.disappearedAt = proc.disappearedAt;
//       let y2 = allocateYPositionAt(proc.disappearedAt, layout);
//       let column = layout.columns[oldProc.columnId];
//       let p = column.processes[column.processes.length-1];
//       if (p.pid != proc.pid) {
//         console.error("expected last process in column to be the updating one", proc, oldProc, column);
//         return;
//       }
// 
//       // calculate y coordinate of start
//       let y1 = findYPositionAt(proc.appearedAt, layout);
//       p.yLength = y2 - y1;
//     }
//   } else {
//     let columnId = selectColumnAvailableAt(proc.appearedAt, layout);
//     let y1 = allocateYPositionAt(proc.appearedAt, layout);
//     layout.processes[proc.pid] = {
//       appearedAt: proc.appearedAt,
//       columnId: columnId,
//       events: []
//     };
//     if (proc.disappearedAt) {
//       // y1 and y2 are not fixed and may change after any update
//       // here needed only to properly calculate length
//       let y2 = allocateYPositionAt(proc.appearedAt, layout);
//       layout.columns[columnId].processes.push({pid: proc.pid, yLength: y2-y1})
//     } else {
//       layout.columns[columnId].processes.push({pid: proc.pid});
//     }
//   }
// };

// let selectColumnAvailableAt = (at, layout) => {
//   if (layout.columnsOrder.length == 0) {
//     
//   }
// };



// let findYPositionAt = (at, layout) => {
//   let y = undefined;
//   let lastAt = undefined; // current candidate for closest position
// 
//   // this function asseses given position and 
//   let checkValue = (at1, y1) = {
//     if (at1 == at) {
//       y = y1;
//       return 'found';
//     } else if (at1 > at) {
//       return 'too_big';
//     } else if (!lastAt) {
//       lastAt = at1;
//       y = y1;
//       return 'good';
//     } else {
//       if (lastAt >= at1) {
//         return 'skip';
//       } else {
//         lastAt = at1;
//         y = y1;
//         return 'good';
//       }
//     }
//   };
// 
//   // first find all suitable processes where position is localted
//   // then search inside each process
//   let suitableProcesses = {};
// 
//   for (let i in layout.columnsOrder) {
//     let column = layout.columns[layout.columnsOrder[i]];
//     // let checked = checkValue(column.startAt, column.startY);
//     // if (column.startAt == at) {
//     //   return column.startY;
//     // }
//     let processOffset = column.startY;
//     for (let j in column.processes) {
//       let p = column.processes[j];
//       let result = checkValue(layout.processes[p.pid].appearedAt, processOffset);
//       if (result == 'found') {
//         return processOffset;
//       } else if (result == 'too_big') {
//         // process in this column starts too late
//         // should check next column
//         break;
//       }
//       case 'found': return processOffset;
//       case 'too_big': break;
//       }
//       
//     }
//   }
// };


// this function walks layout and produces objtree with objects that visible in
// from-to range.
V.produceTree = (layout, from, to) => {
  return {};
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
//       // column doesn't equal to X coordinate, X calculated from it
//       // to have uneven gutter (like 25px column and 4px spacing)
//       // this needed to adjust best spacing
//       column: N,
// 
//       startY: N,
//       stopY: N,
// 
//       spawnedBy: pid,
//       spawnWithLink: true/false,
// 
//       parts: [
//         {type, y, length} // mentioned, mentioned dead, traced
//       ]
//     }
//   },
// 
//   messages: {
//     y: N,
//     fromPid: pid,
//     toPid: pid
//   },
// 
//   links: [
//     {type, pids} // type: link, unlink
//   ]
// };
