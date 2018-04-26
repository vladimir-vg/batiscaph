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

  renderAttr(info, key) {
    if (!info[key]) { return null; }

    const instanceId = this.props.match.params.id;

    switch (key) {
    case 'dictionary':
      return <div key={key}>
        {key}: <MarkedOutput key={key} text={info[key]} instanceId={instanceId} store={this.props.store} />
      </div>;

    default:
      return <div key={key}>
        {key}: <MarkedOutput key={key} isBlock={false} text={info[key]} instanceId={instanceId} store={this.props.store} />
      </div>;
    }
  }

  render() {
    const info = this.props.store.currentProcessInfo;

    if (!info) {
      return <div>Loading...</div>;
    }

    return <div className="ProcessPanel">
      <code>
        <div>
          pid:
          <MarkedOutput isBlock={false}
            text={this.props.match.params.pid}
            instanceId={this.props.match.params.id} store={this.props.store} />
        </div>
        {this.renderAttr(info, 'registered_name')}
        {this.renderAttr(info, 'initial_call')}
        {this.renderAttr(info, 'current_function')}
        {this.renderAttr(info, 'message_queue_len')}
        {this.renderAttr(info, 'links')}
        {this.renderAttr(info, 'monitors')}
        {this.renderAttr(info, 'dictionary')}
      </code>
    </div>;
  }
}
