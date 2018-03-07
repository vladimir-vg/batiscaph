import React from 'react';
import { observer } from 'mobx-react';



@observer
export default class InstancesPage extends React.Component {
  componentDidMount() {
    this.props.store.fetchInstancesList();
  }

  render() {
    console.log(this.props.store.instances);
    return <div>hello list</div>;
  }
}
