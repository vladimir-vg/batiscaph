import { action, observable, extendObservable, computed, toJS } from 'mobx';

import { produceLayout } from './layout';
import { mergeDelta } from './delta';
void(computed); // just to silence eslint, which cannot detect decorators usage



const extractCallbackType = (id) => {
  let m;
  m = /^cowboy (.+)$/.exec(id);
  if (m[1]) { return { id: m[1], type: 'cowboy' }}
  m = /^plug (.+)$/.exec(id);
  if (m[1]) { return { id: m[1], type: 'plug' }}

  throw {
    message: "Unable to extract type from callback id",
    id
  };
};



export default class Store {
  constructor() {
    extendObservable(this, {
      currentInstanceId: null,
      instancesList: [],
      gridEnabled: false,
    });

    this.resetInstanceInfo();

    this.wsSendQueue = [];

    this.onWSMessage = this.onWSMessage.bind(this);
    this.onWSOpen = this.onWSOpen.bind(this);
  }

  resetInstanceInfo() {
    extendObservable(this, {
      isShellConnected: false,
      delta: {
        'plug_requests': (new Map()),
        'cowboy_requests': (new Map()),
        'erlang_processes': (new Map()),
        'lager_events': (new Map()),

        // initialize as Map, in order to be able to listen
        // for new item
        'shell_commands': (new Map()),
        'erlang_processes_info': (new Map()),
        'erlang_processes_info_binaries': (new Map()),
      },
      selectedProcessPid: null,
      hoveredProcessPid: null,
      selectedCallbackId: null,
      hoveredCallbackId: null,
      hoveredLogId: null,

      // for full request info
      // we need use Map to make MobX react to dynamically added keys
      // ordinary object won't work
      reqsDetails: new Map(),
    });
  }

  // to be used for debug from browser console
  @action
  toggleGrid() {
    this.gridEnabled = !this.gridEnabled;
  }



  @action
  fetchInstancesList() {
    fetch('/api/instances?user_id=1')
      .then((response) => response.json())
      .then(action((json) => {
        this.instancesList.replace(json);
      }));
  }

  @computed
  get layout() {
    return produceLayout(this.delta);
  }

  // sorted list of ready to display reqs
  @computed
  get httpRequestsList() {
    const reqs1 = Array.from(this.delta['plug_requests'].values());
    const reqs2 = Array.from(this.delta['cowboy_requests'].values());
    const reqs = reqs1.concat(reqs2);
    reqs.sort((a, b) => {
      let aAt, bAt;
      if ('StartedAt' in a) {
        aAt = a.StartedAt;
      } else if (a.init && 'StartedAt' in a.init) {
        aAt = a.init.StartedAt;
      }
      if ('StartedAt' in b) {
        bAt = b.StartedAt;
      } else if (b.init && 'StartedAt' in b.init) {
        bAt = b.init.StartedAt;
      }
      return aAt > bAt ? 1 : -1;
    });
    return reqs;
  }

  @action
  consumeDelta(delta) {
    // to distinguish requests, when they are merged into one list
    for (const id in delta['cowboy_requests']) {
      delta['cowboy_requests'][id].Id1 = `cowboy ${id}`;
    }
    for (const id in delta['plug_requests']) {
      delta['plug_requests'][id].Id1 = `plug ${id}`;
    }

    // make new delta also observable, to avoid having unobserved parts
    // in observed delta
    mergeDelta({oldDelta: this.delta, newDelta: observable(delta)});
  }

  @action
  selectCallback(id) {
    this.selectedCallbackId = id;
    if (id) {
      const { id: id1, type } = extractCallbackType(id);
      this.ensureRequestInfoFetch(id1, type);
    }
  }

  @action
  hoverCallback(id) {
    this.hoveredCallbackId = id;
  }

  @action
  ensureRequestInfoFetch(id, type) {
    if (!this.currentInstanceId) {
      throw {
        message: "Undefined current instance while fetching request info",
        instanceId: this.currentInstanceId, requestId: id
      };
    }

    if (this.reqsDetails.has(id) && this.reqsDetails.get(id).StoppedAt) {
      return; // request was fetched before, and request was finished
    }

    let url = null;
    if (type === 'plug') {
      url = `/api/instances/${this.currentInstanceId}/plug_requests/${id}`;
    } else if (type === 'cowboy') {
      url = `/api/instances/${this.currentInstanceId}/cowboy_requests/${id}`;
    }

    fetch(url)
      .then((response) => response.json())
      .then(action((json) => {
        this.reqsDetails.set(id, json);
      }));
  }

  @computed
  get selectedReqInfo() {
    if (!this.selectedCallbackId) { return null; }
    const { id, _type } = extractCallbackType(this.selectedCallbackId);
    if (!this.reqsDetails.has(id)) { return 'loading'; }

    return this.reqsDetails.get(id);
  }

  @action
  selectProcess(pid) {
    this.selectedProcessPid = pid;
    if (pid) {
      this.wsSend('subscribe_to_process_info', { pid, instance_id: this.currentInstanceId });
    } else {
      // TODO: unsubscribe from process_info
    }
  }

  @action
  hoverProcess(id) {
    this.hoveredProcessPid = id;
  }

  @computed
  get currentProcessInfo() {
    if (!this.selectedProcessPid) {
      throw {
        message: "Expected selected process pid to be present while fetching current process info",
        selectedProcessPid: this.selectedProcessPid
      };
    }

    const info = this.delta['erlang_processes_info'].get(this.selectedProcessPid);
    if (!info) return null;

    const { Changes } = info;
    const timestamps = Object.keys(Changes);
    timestamps.sort();

    // collapse all changes into latest one
    let result = {};
    for (const i in timestamps) {
      const at = timestamps[i];
      result = Object.assign(result, Changes[at]);
    }

    result.ExitReason = info.ExitReason;

    return result;
  }

  @computed
  get currentProcessBinaries() {
    if (!this.selectedProcessPid) {
      throw {
        message: "Expected selected process pid to be present while fetching current process info",
        selectedProcessPid: this.selectedProcessPid
      };
    }

    const info = this.delta['erlang_processes_info_binaries'].get(this.selectedProcessPid);
    if (!info) return null;

    const { Changes } = info;
    const timestamps = Object.keys(Changes);
    timestamps.sort();

    // collapse all changes into latest one
    let result = {};
    for (const i in timestamps) {
      const at = timestamps[i];
      result = Object.assign(result, Changes[at]);
    }

    // skip binaries that present in history, but now gone
    for (const at in result) {
      if (!result[at]) { delete result[at]; }
    }

    return result;
  }



  @computed
  get orderedLogs() {
    const result = [];
    const logs = Array.from(this.delta['lager_events'].values());
    logs.sort(({ At: aAt }, { At: bAt }) => {
      return aAt > bAt ? 1 : -1;
    });
    return logs;
  }

  @action
  hoverLogEvent(id) {
    this.hoveredLogId = id;
  }



  @action
  subscribeToInstance(id) {
    this.currentInstanceId = id;
    if (!this.socket) { this.connectToWebsocket(); }
    this.wsSend('subscribe_to_instance', {id: id});
  }

  @action
  unsubscribeFromInstance(id) {
    if (this.currentInstanceId == id) { this.currentInstanceId = null; }
    this.wsSend('unsubscribe_from_instance', { id });
    this.resetInstanceInfo();
  }

  ensureShellConnected() {
    if (!this.currentInstanceId) {
      throw {
        message: "Expected instance id to be present while connecting to shell",
        instanceId: this.currentInstanceId
      };
    }

    if (!this.isShellConnected) {
      this.wsSend('connect_to_shell', {id: this.currentInstanceId});
    }
  }

  sendShellInput(text) {
    this.wsSend('shell_input', { id: this.currentInstanceId, text });
  }

  @computed
  get shellCommands() {
    const cmds = Array.from(this.delta['shell_commands'].values());
    cmds.sort((a, b) => a.At > b.At ? 1 : -1);
    const cmds1 = cmds.map(({ At, Input, Outputs, Pid, Prompt }) => {
      // Chrome bug that breaks Object.values on MobX objects
      // https://github.com/mobxjs/mobx/issues/1504
      // mandatory convert it using toJS to workaround
      const Outputs1 = Object.values(toJS(Outputs));
      Outputs1.sort((a, b) => a.At > b.At ? 1 : -1);
      return { Outputs: Outputs1, At, Input, Pid, Prompt };
    });
    return cmds1;
  }



  connectToWebsocket() {
    const url = new URL(window.location.href);
    const proto = url.protocol === 'http:' ? 'ws' : 'wss';
    const wsurl = `${proto}://${url.host}/websocket`;
    this.socket = new WebSocket(wsurl);
    this.socket.addEventListener('message', this.onWSMessage);
    this.socket.addEventListener('open', this.onWSOpen);
  }

  @action
  onWSMessage({ data }) {
    const re = /^([^ ]+) (.*)/; // split by only one space
    const match = re.exec(data);
    let method, payload;
    if (match) {
      method = match[1];
      payload = match[2];
    } else {
      method = data;
      payload = null;
    }

    switch (method) {
    case 'delta': this.consumeDelta(JSON.parse(payload)); break;
    case 'connected_to_shell': this.isShellConnected = true; break;

    default:
      throw {
        message: "Unknown message from server",
        method, payload
      };
    }
  }

  onWSOpen() {
    while (this.wsSendQueue.length !== 0) {
      // take from queue from beginning
      const { method, arg } = this.wsSendQueue.shift();
      this.socket.send(method + ' ' + JSON.stringify(arg));
    }
  }

  wsSend(method, arg) {
    if (this.socket.readyState == WebSocket.CONNECTING) {
      this.wsSendQueue.push({ method, arg });
    } else if (this.socket.readyState == WebSocket.OPEN) {
      this.socket.send(method + ' ' + JSON.stringify(arg));
    } else {
      throw {
        message: "Wrong websocket state when attempted to send",
        method, arg, state: this.socket.readyState
      };
    }
  }
}
