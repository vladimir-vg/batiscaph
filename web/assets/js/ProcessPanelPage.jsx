import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';



@inject("store") @observer
export default class ProcessPanelPage extends React.Component {
  componentDidMount() {
    this.props.store.subscribeToProcessInfo(this.props.match.params.pid);
  }

  componentWillReceiveProps(props) {
    if (props.match.params.pid != this.props.match.params.pid) {
      this.props.store.subscribeToProcessInfo(props.match.params.pid);
    }
  }

  render() {
    return <div />;
  }
}
