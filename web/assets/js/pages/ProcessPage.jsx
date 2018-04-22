import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';

import MarkedOutput from '../components/MarkedOutput';



@inject("store") @observer
export default class ProcessPage extends React.Component {
  constructor() {
    super();
    this.renderAttr = this.renderAttr.bind(this);
    this.hoverProcess = this.hoverProcess.bind(this);
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

  hoverProcess(id) { this.props.store.hoverProcess(id); }

  renderAttr([key, value]) {
    const instanceId = this.props.match.params.id;

    if (key === 'dictionary') {
      return <div key={key}>
        {key}: <MarkedOutput key={key} text={value} instanceId={instanceId} store={this.props.store} />
      </div>;
    }
    return <div key={key}>
      {key}: <MarkedOutput key={key} isBlock={false} text={value} instanceId={instanceId} store={this.props.store} />
    </div>;
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
