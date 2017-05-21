
let get = (keys, values, key) => {
  let index = keys.indexOf(key);
  if (index == -1) {
    console.error("not found key in keys: ", key, keys, values);
    return null;
  }
  return values[index];
};



// this function selects best from available columns
// or allocate new column
let insertProc = (proc, tree) => {
  let column;
  if (tree._availColumns.length == 0) {
    let x = (Object.keys(tree._currentColumns).length + tree._availColumns.length);
    column = {x: x};
  } else {
    column = tree._availColumns.pop();
  }

  column.pid = proc.pid;
  tree._currentColumns[proc.pid] = column;

  // currently just simply enumerate them without aggregation
  proc.startedY = tree._currentRow.y;
  tree._currentRow.y += 1;
  proc.x = column.x;

  tree.procs[proc.pid] = proc;
};

let spawnProc = (keys, values, tree) => {
  let at = get(keys, values, 'at');
  let atMcs = get(keys, values, 'at_mcs');

  let pid = get(keys, values, 'pid');
  let parent = get(keys, values, 'pid_arg');
  let mfa = get(keys, values, 'mfa');

  let proc = {startedAt: at, startedAtMcs: atMcs, pid: pid, parent: parent, mfa: mfa};

  if (tree.procs[pid]) {
    console.error("process is already saved before spawn event processed", tree.procs[pid], keys, values);
    return;
  }

  insertProc(proc, tree);
};



let exitProc = (keys, values, tree) => {
  let at = get(keys, values, 'at');
  let atMcs = get(keys, values, 'at_mcs');

  let pid = get(keys, values, 'pid');
  let reason = get(keys, values, 'term');

  if (!tree.procs[pid]) {
    console.error("exit event but not spawned proc in tree", keys, values);
    return;
  }

  if (!tree._currentColumns[pid]) {
    console.error("exit event but no occupied proc in tree", keys, values);
    return;
  }

  let column = tree._currentColumns[pid];
  delete tree._currentColumns[pid];
  tree._availColumns.push(column);

  // sort to make lower columns be taken first
  tree._availColumns.sort((a,b) => {
    if (a.x < b.x) {
      return -1;
    } else if (a.x > b.x) {
      return 1;
    }
    return 0;
  });

  tree.procs[pid].stoppedAt = at;
  tree.procs[pid].stoppedAtMcs = atMcs;
  tree.procs[pid].reason = reason;
  tree.procs[pid].stoppedY = tree._currentRow.y;

  tree._currentRow.y += 1;
};



// this procedure destructively changes tree
let processEvent = (keys, values, tree) => {
  switch (get(keys, values, 'type')) {
  case 'spawn': spawnProc(keys, values, tree); break;
  case 'exit': exitProc(keys, values, tree); break;
  }
};



V.processEvents = function (keys, rows) {
  var tree = {
    // these are state fields that used during processing
    _availColumns: [], // free columns that might be occupied by new processes
    _currentColumns: {}, // currently occupied columns, {pid: column}
    _currentRow: {y: 0},

    // these are output fields that later gonna be used for visualization
    procs: {},
  };

  for (let i in rows) {
    processEvent(keys, rows[i], tree);
  }

  tree.maxY = tree._currentRow.y;

  return tree;
};


