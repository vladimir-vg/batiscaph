import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';
import { scaleLog } from 'd3-scale';

import MarkedOutput from '../components/MarkedOutput';



const BINARY_MAX_ALLOWED_WIDTH = 300; // hardcoded in css as min-height for side panel
const KILOBYTE = 1024;
const MEGABYTE = 1024*1024;
const GIGABYTE = 1024*1024*1024;

const binaryWidthScale = (() => {
  const oneFifthOfSpace = Math.floor(BINARY_MAX_ALLOWED_WIDTH*1/5);
  const twoThirdsOfSpace = Math.floor(BINARY_MAX_ALLOWED_WIDTH*2/3);

  const kbScale = scaleLog()
    .domain([KILOBYTE, MEGABYTE])
    .range([1, oneFifthOfSpace]);

  const mbScale = scaleLog()
    .domain([MEGABYTE, MEGABYTE*512])
    .range([oneFifthOfSpace, twoThirdsOfSpace]);

  const gbScale = scaleLog()
    .domain([MEGABYTE*512, GIGABYTE])
    .range([twoThirdsOfSpace, BINARY_MAX_ALLOWED_WIDTH]);

  return (size) => {
    if (size <= KILOBYTE) { return 1; }
    if (size >= GIGABYTE) { return BINARY_MAX_ALLOWED_WIDTH; }
    if (size <= MEGABYTE) { return Math.floor(kbScale(size)); }

    // likely that's what most of the time people gonna deal with
    // so use this space in the most detailed way
    if (size <= MEGABYTE*512) { return Math.floor(mbScale(size)); }

    // only size larger than half-gigabyte left
    return Math.floor(gbScale(size));
  }
})();



const formatBinarySize = (size) => {
  if (size < KILOBYTE) { return "<1KB"; }
  if (size < MEGABYTE) { return `${Math.floor(size/KILOBYTE)}KB`; }
  if (size < GIGABYTE) { return `${Math.floor(size/MEGABYTE)}MB`; }
  return `${(size/GIGABYTE).toFixed(2)}GB`;
}


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

  renderBinaries() {
    const binaries = this.props.store.currentProcessBinaries;
    if (!binaries) return null;

    const result = [];

    for (const key in binaries) {
      const [size, refCount] = binaries[key].split(' ');
      const width = binaryWidthScale(size);
      let text = "";
      if (width > 30) { text = formatBinarySize(size); }
      result.push(<div key={key} style={{ width }}>{text}</div>);
    }

    return <div>
      <h1>ref-counted binaries</h1>
      <div className="binary-bricks">{result}</div>
    </div>;
  }

  render() {
    const info = this.props.store.currentProcessInfo;

    if (!info) {
      return <div>Loading...</div>;
    }

    return <div className="ProcessPage">
      <code>
        <div>
          pid:&nbsp;
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

        {this.renderBinaries()}
      </code>
    </div>;
  }
}
