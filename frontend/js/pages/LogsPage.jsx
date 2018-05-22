import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';
import moment from 'moment';

import MarkedOutput from '../components/MarkedOutput';



const formatTimestamp = (mcs) => {
  const m = moment(Math.floor(mcs/1000));
  const ms = Math.floor((mcs % (1000*1000))/1000);
  return `${m.format('HH:mm:ss')}.${ms}`;
};



@inject("store") @observer
export default class LogsPage extends React.Component {
  constructor() {
    super();

    this.renderItem = this.renderItem.bind(this);
    this.hoverLogEvent = this.hoverLogEvent.bind(this);
  }

  componentWillUnmount() {
    this.props.store.hoverLogEvent(null);
  }

  hoverLogEvent(id) { this.props.store.hoverLogEvent(id); }

  renderItem({ Pid, At, Id, text, module, line, 'function': fun }) {
    const key = `${At} ${Pid}`;
    const instanceId = this.props.match.params.id;
    const text1 = `${formatTimestamp(At)} ${Pid} ${module}:${line} ${text}`;

    let className = (this.props.store.hoveredLogId === Id) ? "hovered" : '';

    return <div key={key}
        onMouseEnter={this.hoverLogEvent.bind(this, Id)}
        onMouseLeave={this.hoverLogEvent.bind(this, null)}>

      <MarkedOutput className={className} isBlock={false}
        text={text1} instanceId={instanceId} store={this.props.store} />
    </div>
  }

  render() {
    const logs = this.props.store.orderedLogs;

    return <div className="LogsPage">
      {logs.map(this.renderItem)}
    </div>;
  }
}
