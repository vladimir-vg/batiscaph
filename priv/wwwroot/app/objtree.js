// In this file contains code related to manipulating objects tree,
// that should be rendered on the map.



// this structure contains columns, processes and events
// it does not contain absolute coords for display, only relative values
// it can be easily manipulated and updated, columns might be reordered
// let layout = {
//   processes: {
//     pid: {
//       columnId: id,
//       events: []
//     }
//   },
// 
//   columns: [
//     {
//       columnId: id,
//       mentionsOnly: false,
//       startY: N,
//       processes: [
//         {pid, yLength, yBottomMargin}
//       ]
//     }
//   ]
// }



// Rules of layout:
//
// * Spawned children should be always to the right of parent.
// * Mentions of the processes should be located in leftmost column.
//   If process mentioned twice and two mentions create connection,
//   then process should be moved to new right column
//   If mentioned process was expanded, then it also moved into new column
// * spawn and following link event should collapse into one visible event
//



// this function applies new data from server to existing layout
// it should add new columns if needed and reuse old
//
// processes and events in delta are expected to be ordered
// processes must be ordered by first apperance
//
// this function destructively updates layout
V.updateLayout = (delta, layout) => {
  for (var i in delta.processes) {
    updateProcessInLayout(delta.processes[i], layout);
  }
  return layout;
};

let updateProcessInLayout = (proc, layout) => {
  
};



// this function walks layout and produces objtree with objects that visible in
// from-to range.
V.produceTree = (from, to, layout) => {
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
