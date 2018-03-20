import { action, observable, extendObservable, computed } from 'mobx';

import { produceLayout } from './layout';
import { mergeDelta } from './delta';
void(computed); // just to silence eslint, which cannot detect decorators usage



export default class Store {
  constructor() {
    extendObservable(this, {
      currentInstanceId: null,
      instancesList: [],
      delta: {
        'plug:requests': null,
      },
      selectedRequestId: null,

      // for full request info
      // we need use Map to make MobX react to dynamically added keys
      // ordinary object won't work
      reqsDetails: new Map(),
    });

    this.wsSendQueue = [];

    this.onWSMessage = this.onWSMessage.bind(this);
    this.onWSOpen = this.onWSOpen.bind(this);
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
    const reqs = Object.values(this.delta['plug:requests'] || {});
    reqs.sort(({StartedAt: a}, {StartedAt: b}) => a < b ? 1 : -1);
    return reqs;
  }

  @action
  consumeDelta(delta) {
    // make new delta also observable, to avoid having unobserved parts
    // in observed delta
    mergeDelta({oldDelta: this.delta, newDelta: observable(delta)});
  }

  @action
  onSelectRequest(id) {
    if (id === null) {
      this.selectedRequestId = null;
    } else {
      this.selectedRequestId = id;
      this.ensureRequestInfoFetch(id);
    }
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

    fetch(`${window.API_URL}/instances/${this.currentInstanceId}/plug-requests/${id}`)
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
    this.wsSend('unsubscribe_from_instance', {id: id});
  }

  connectToWebsocket() {
    const url = new URL(window.API_URL);
    const proto = url.protocol === 'http:' ? 'ws' : 'wss';
    const wsurl = `${proto}://${url.host}/websocket`;
    this.socket = new WebSocket(wsurl);
    this.socket.addEventListener('message', this.onWSMessage);
    this.socket.addEventListener('open', this.onWSOpen);
  }

  onWSMessage({ data }) {
    const re = /^([^ ]+) (.*)/; // split by only one space
    const match = re.exec(data);
    const [_, method, payload] = match;
    // const method = match[1];
    // const payload = match[2];

    switch (method) {
    case 'delta': this.consumeDelta(JSON.parse(payload)); break;

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
