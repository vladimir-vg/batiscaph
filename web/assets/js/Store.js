import { action, observable, extendObservable, computed } from 'mobx';

import { produceLayout } from './layout';
import { mergeDelta } from './delta';
void(computed); // just to silence eslint, which cannot detect decorators usage



export default class Store {
  constructor() {
    extendObservable(this, {
      currentInstanceId: null,
      isShellConnected: false,
      instancesList: [],
      delta: {
        'plug-requests': null,
        'cowboy-requests': null,
        'erlang-processes': null,
        'shell-events': null,
      },
      selectedRequestId: null,
      hoveredRequestId: null,
      gridEnabled: false,

      // for full request info
      // we need use Map to make MobX react to dynamically added keys
      // ordinary object won't work
      reqsDetails: new Map(),
    });

    this.wsSendQueue = [];

    this.onWSMessage = this.onWSMessage.bind(this);
    this.onWSOpen = this.onWSOpen.bind(this);
  }

  // to be used for debug from browser console
  @action
  toggleGrid() {
    this.gridEnabled = !this.gridEnabled;
  }



  @action
  fetchInstancesList() {
    fetch(window.API_URL + '/instances?user_id=1')
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
    const reqs1 = Object.values(this.delta['plug-requests'] || {});
    const reqs2 = Object.values(this.delta['cowboy-requests'] || {});
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
    // make new delta also observable, to avoid having unobserved parts
    // in observed delta
    mergeDelta({oldDelta: this.delta, newDelta: observable(delta)});
  }

  @action
  onRequestSelect(id) {
    this.selectedRequestId = id;
    if (id) { this.ensureRequestInfoFetch(id); }
  }

  @action
  onRequestHover(id) {
    this.hoveredRequestId = id;
  }

  @action
  ensureRequestInfoFetch(id) {
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
    if (id in this.delta['plug-requests']) {
      url = `${window.API_URL}/instances/${this.currentInstanceId}/plug-requests/${id}`;
    } else if (id in this.delta['cowboy-requests']) {
      url = `${window.API_URL}/instances/${this.currentInstanceId}/cowboy-requests/${id}`;
    }

    fetch(url)
      .then((response) => response.json())
      .then(action((json) => {
        this.reqsDetails.set(id, json);
      }));
  }

  @computed
  get selectedReqInfo() {
    if (!this.selectedRequestId) { return null; }
    if (!this.reqsDetails.has(this.selectedRequestId)) { return 'loading'; }

    return this.reqsDetails.get(this.selectedRequestId);
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
    const cmds = Object.values(this.delta['shell-commands'] || {});
    cmds.sort((a, b) => a.At > b.At ? 1 : -1);
    const cmds1 = cmds.map((e) => {
      const values = Object.values(e.Outputs);
      values.sort((a, b) => a.At > b.At ? 1 : -1);
      e.Outputs = values;
      return e;
    })
    return cmds1;
  }

  connectToWebsocket() {
    const url = new URL(window.API_URL);
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
