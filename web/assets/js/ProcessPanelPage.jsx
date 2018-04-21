import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';



@inject("store") @observer
export default class ProcessPanelPage extends React.Component {
  constructor() {
    super();
    this.renderAttr = this.renderAttr.bind(this);
  }

  componentWillMount() {
    this.props.store.selectProcess(this.props.match.params.pid);
  }

  componentWillUnmount() {
    this.props.store.selectProcess(null);
  }

  componentWillReceiveProps(props) {
    if (props.match.params.pid != this.props.match.params.pid) {
      this.props.store.selectProcess(props.match.params.pid);
    }
  }

  renderAttr([key, value]) {
    if (key === 'dictionary') {
      return <div key={key}>{key}: <pre>{value}</pre></div>;
    }
    return <div key={key}>{key}: {value}</div>;
  }

  render() {
    const info = this.props.store.currentProcessInfo;

    if (!info) {
      return <div>Loading...</div>;
    }

    return <div className="ProcessPanel">
      <code>
        {this.renderAttr(['pid', this.props.match.params.pid])}
        {Object.entries(info).map(this.renderAttr)}
      </code>
    </div>;
  }
}
