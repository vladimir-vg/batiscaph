import { observable, action } from 'mobx';



export default class Store {
  constructor() {
    this.instances = observable([]);
  }

  @action
  fetchInstancesList() {
    fetch(window.API_URL + '/instances?user_id=1')
      .then((response) => response.json())
      .then(action((json) => {
        this.instances.replace(json);
      }));
  }
}
