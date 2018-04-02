import React from 'react';
import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import SvgView from './SvgView';
import RequestsList from './RequestsList';



const hGap = 4;
const colWidth = 10;
const rowHeight = 3;
const grid = {
  xColStart: (x) => { return x*(colWidth+hGap) + hGap },
  xColWidth: colWidth,
  yRowAt: (y) => { return y*rowHeight },
  yRowHeight: rowHeight,
};



@inject("store") @observer
export default class InstancePage extends React.Component {
  constructor() {
    super();

    this.onRequestSelect = this.onRequestSelect.bind(this);
    this.onRequestHover = this.onRequestHover.bind(this);
    this.renderElement = this.renderElement.bind(this);

    // TODO: listen to resize event, update height accordingly
    const viewportHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
    this.state = {
      viewportHeight
    };
  }

  componentWillMount() {
    this.props.store.subscribeToInstance(this.props.match.params.id);
  }

  componentWillUnmount() {
    this.props.store.unsubscribeFromInstance(this.props.match.params.id);
  }

  onRequestSelect(id) { this.props.store.onRequestSelect(id); }
  onRequestHover(id) { this.props.store.onRequestHover(id); }

  renderGrid() {
    // return null;

    const cols = [];
    const rows = [];
    for (let i = -100; i < 700; i++) {
      cols.push(<rect key={'col'+i}
        style={{fill: 'rgba(255,0,0,0.08)'}}
        x={grid.xColStart(i)} y={grid.yRowAt(-100)}
        width={grid.xColWidth} height={2000} />);
    }
    for (let i = -100; i < 300; i += 2) {
      rows.push(<rect key={'row'+i}
        style={{fill: 'rgba(255,0,0,0.08)'}}
        x={-200} y={grid.yRowAt(i)} width={2000} height={grid.yRowHeight} />);
    }

    return <g>{cols}{rows}</g>;
  }

  renderElement(e) {
    const { selectedRequestId, hoveredRequestId } = this.props.store;
    const { onRequestSelect, onRequestHover } = this; // take wrapped functions
    const storeProps = { onRequestSelect, onRequestHover, selectedRequestId, hoveredRequestId };
    const { id, key, Component, ...elementProps} = e;
    return <Component key={key} grid={grid} {...storeProps} id={id} {...elementProps} />;
  }

  render() {
    const reqs = this.props.store.layout.HttpReq || [];
    const procs = this.props.store.layout.Process || [];

    // for some reason setting viewportHeight for div height creates scrollbar
    // make it disappear using overflow: hidden
    return <div className="InstancePage" style={{height: this.state.viewportHeight, overflow: 'hidden'}}>
      <div className="map-container">
        <SvgView padding={100} paddingLeft={100} paddedWidth={300} paddedHeight={1000}>
          {this.renderGrid()}
          <g>{reqs.map(this.renderElement)}</g>
          <g>{procs.map(this.renderElement)}</g>
        </SvgView>
      </div>
      <div className="extra-info-container">
        <RequestsList
          reqs={this.props.store.httpRequestsList} selectedReqInfo={this.props.store.selectedReqInfo}
          selectedRequestId={this.props.store.selectedRequestId} hoveredRequestId={this.props.store.hoveredRequestId}
          onRequestSelect={this.onRequestSelect} onRequestHover={this.onRequestHover} />
      </div>
    </div>;
  }
}
