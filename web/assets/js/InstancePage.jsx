import React from 'react';
import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage



@inject("store") @observer
export default class InstancePage extends React.Component {
  componentWillMount() {
    this.props.store.subscribeToInstance(this.props.match.params.id);
  }

  componentWillUnmount() {
    this.props.store.unsubscribeFromInstance(this.props.match.params.id);
  }

  render() {
    return <div>
      Hello {'' +   Object.keys(this.props.store.layout)}
    </div>;
  }
}
