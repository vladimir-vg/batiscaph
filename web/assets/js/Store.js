import { observable, action } from 'mobx';



export default class Store {
  constructor() {
    this.instancesList = observable([]);
    
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

    this.wsSend('subscribe-to-instance', {id: id});
  }

  unsubscribeFromInstance(_id) {
    this.wsSend('unsubscribe-from-instance', {id: id});
  }

  connectToWebsocket() {
    const url = new URL(window.API_URL);
    const proto = url.protocol === 'http:' ? 'ws' : 'wss';
    const wsurl = `${proto}://${url.host}/socket`;
    this.socket = new WebSocket(wsurl);
    this.socket.addEventListener('message', this.onWSMessage);
    this.socket.addEventListener('open', this.onWSOpen);
  }

  onWSMessage() {
    
  }

  onWSOpen() {
    
  }

  wsSend(method, arg) {
    this.socket.send(method + ' ' + JSON.stringify(arg));
  }
}
