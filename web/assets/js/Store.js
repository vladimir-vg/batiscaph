import { observable, action } from 'mobx';



export default class Store {
  constructor() {
    this.instancesList = observable([]);
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

  subscribeToInstance(id) {
    if (!this.socket) { this.connectToWebsocket(); }

    this.wsSend('subscribe_to_instance', {id: id});
  }

  unsubscribeFromInstance(_id) {
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

  onWSMessage() {
    
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
