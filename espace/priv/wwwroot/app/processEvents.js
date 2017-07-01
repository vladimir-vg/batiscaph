
let get = (keys, values, key) => {
  if (!values) return null;

  if (keys == 'json') {
    return values[key];
  }

  let index = keys.indexOf(key);
  if (index == -1) {
    console.error("not found key in keys: ", key, keys, values);
    return null;
  }

  return values[index];
};



let availableColumnForProc = (proc, tree) => {
  if (tree._availColumns.length == 0) return null;

  let ancestorsMaxX = proc.ancestors.reduce((acc, pid) => {
    return Math.max(acc, tree.procs[pid].x);
  }, -1);

  // take any free column to the right from ancestors
  // make procs spawn from left to right

  for (let i in tree._availColumns) {
    let column = tree._availColumns[i];
    if (column.x > ancestorsMaxX) {
      tree._availColumns.splice(i, 1);
      return column;
    }
  }

  return null;
};


// this function selects best from available columns
// or allocate new column
let insertProc = (proc, tree) => {
  let column = availableColumnForProc(proc, tree);
  if (!column) {
    let x = (Object.keys(tree._currentColumns).length + tree._availColumns.length);
    column = {x: x};
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

  if (tree.procs[parent]) {
    proc.ancestors = tree.procs[parent].ancestors.slice();
    proc.ancestors.unshift(parent); // prepend current parent
  } else {
    proc.ancestors = [];
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



// this function may form several consecutive events into one shell io event
let addShellIO = (keys, rows, i, tree) => {
  let currentEvent = null;

  while (true) {
    let values = rows[i];
    let prev = rows[i-1];
    let next = rows[i+1];

    let type = get(keys, values, 'type');
    let at = get(keys, values, 'at');
    let atMcs = get(keys, values, 'at_mcs');
    let prompt = get(keys, values, 'prompt');
    let message = get(keys, values, 'message');

    if (type == 'module_stored') {
      let name = get(keys, values, 'atom');
      let size = get(keys, values, 'size');
      let hash = get(keys, values, 'hash');
      prompt = "erl";
      message = "uploaded " + name + ".erl, " + size + " bytes, md5 hash: " + hash;
    }

    if (['shell_output', 'shell_input', 'module_stored'].indexOf(type) == -1) {
      break;
    }

    if (!currentEvent) {
      currentEvent = {at: at, atMcs: atMcs, type: type};
      currentEvent.blocks = [{lines: message.trim().split("\n"), prompt: prompt}];
    } else {
      currentEvent.blocks.push({lines: message.trim().split("\n"), prompt: prompt});
    }

    if (
      (get(keys, next, 'type') == 'shell_output' && type == 'shell_output')
      || (get(keys, next, 'type') == 'module_stored' && type == 'module_stored')
    ) {
      i += 1;
    } else {
      break;
    }
  }

  let linesCount = currentEvent.blocks.reduce(function (acc, block) {
    return acc + block.lines.length;
  }, 0);

  currentEvent.height = linesCount*V.SHELL_LINE_HEIGHT;
  currentEvent.length = Math.trunc(currentEvent.height/V.CELL_HEIGHT); // in cells

  currentEvent.y = tree._currentRow.y;
  tree._currentRow.y += currentEvent.length+1;

  tree.shellIO.push(currentEvent);

  return i;
};



let addMessageSend = (keys, values, tree) => {
  let at = get(keys, values, 'at');
  let atMcs = get(keys, values, 'at_mcs');
  let pidFrom = get(keys, values, 'pid');
  let pidTo = get(keys, values, 'pid_arg');
  let atomTo = get(keys, values, 'atom');
  let term = get(keys, values, 'term');

  // ignore message if both sender and received are not displayed
  if (!(pidFrom in tree.procs) && !(pidTo in tree.procs)) return;
  let e = {at: at, atMcs: atMcs, term: term, from: pidFrom, to: pidTo, toAtom: atomTo};

  if (atomTo && atomTo in tree.registeredAtoms) {
    e.to = tree.registeredAtoms[atomTo];
  }

  e.y = tree._currentRow.y;
  tree._currentRow.y += 1;

  tree.sends.push(e);
};



let registerAtom = (keys, values, tree) => {
  let at = get(keys, values, 'at');
  let atMcs = get(keys, values, 'at_mcs');
  let pid = get(keys, values, 'pid');
  let atom = get(keys, values, 'atom');

  tree.procs[pid].atom = atom;
  tree.registeredAtoms[atom] = pid;
};



// this procedure destructively changes tree
let processEvent = (keys, rows, i, tree) => {
  let values = rows[i];
  let prev = rows[i-1];
  let next = rows[i+1];
  switch (get(keys, values, 'type')) {
  case 'spawn': spawnProc(keys, values, tree); break;
  case 'exit': exitProc(keys, values, tree); break;
  case 'send': addMessageSend(keys, values, tree); break;
  case 'register': registerAtom(keys, values, tree); break;

  case 'shell_input_expected':
    let prompt = get(keys, values, 'prompt');
    tree.currentPrompt = prompt;
    break;

  case 'shell_input':
    i = addShellIO(keys, rows, i, tree);
    tree.currentPrompt = null; // input was accepted, wait for new prompt
    break;

  case 'shell_output':
  case 'module_stored':
    i = addShellIO(keys, rows, i, tree);
    break;
  }
  return i;
};



V.processEvents = function (tree, rows, keys) {

  if (!tree) {
    tree = {
      // these are state fields that used during processing
      _availColumns: [], // free columns that might be occupied by new processes
      _currentColumns: {}, // currently occupied columns, {pid: column}
      _currentRow: {y: 0},

      // not null if input is expected by remote shell
      currentPrompt: null,

      // these are output fields that later gonna be used for visualization
      procs: {},

      registeredAtoms: {}, // atom: pid

      // sorted list of events with length (in cells)
      shellIO: [],

      sends: [],
    };
  }

  // create new reference for tree, to allow compare old tree prop with new
  tree = Object.assign({}, tree);

  for (let i = 0; i < rows.length; i += 1) {
    // it may process several consecutive events
    // increase i counter independenly
    i = processEvent(keys, rows, i, tree);
  }

  tree.maxY = tree._currentRow.y;
  tree.maxX = (Object.keys(tree._currentColumns).length + tree._availColumns.length);

  return tree;
};


