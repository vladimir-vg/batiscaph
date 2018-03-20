import React from 'react';
import { observer, inject } from 'mobx-react';
import PropTypes from 'prop-types';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import SvgView from './SvgView';
import HttpReq from './elements/HttpReq';



const hGap = 4;
const colWidth = 10;
const rowHeight = 3;
const grid = {
  xColStart: (x) => { return x*(colWidth+hGap) + hGap },
  xColWidth: colWidth,
  yRowAt: (y) => { return y*rowHeight },
  yRowHeight: rowHeight,
};



class RequestsInfo extends React.Component {
  constructor() {
    super();

    // only affects scroll, shouldn't be put into state
    // not part of the renderable state
    this.stickToBottom = true;

    this.onItemSelect = this.onItemSelect.bind(this);
    this.clearSelection = this.onItemSelect.bind(this, null);
    this.renderItem = this.renderItem.bind(this);
  }

  componentDidUpdate() {
    if (this.stickToBottom) {
      this.containerRef.scrollTop = this.containerRef.scrollHeight - this.containerRef.clientHeight;
    }
  }

  onItemSelect(id) {
    this.props.onItemSelect(id);
  }

  renderItem({ id, method, path, resp_code }) {
    return <tr key={id} onClick={this.onItemSelect.bind(this, id)}>
      <td>{method}</td>
      <td>{path}</td>
      <td>{resp_code}</td>
    </tr>;
  }

  renderSelectedRequest({ topOffest }) {
    if (!this.props.selectedReqInfo) { return null; }

    if (this.props.selectedReqInfo === 'loading') {
      return <div style={{position: 'absolute', bottom: 0, top: topOffest, backgroundColor: 'white', zIndex: 1, width: '100%'}}>
        loading...
        <button onClick={this.clearSelection}>x</button>
      </div>;
    }

    return <div style={{position: 'absolute', bottom: 0, top: topOffest, backgroundColor: 'white', zIndex: 1, width: '100%'}}>
      huy
      <button onClick={this.clearSelection}>x</button>
    </div>;
  }

  render() {
    const topOffest = 60;
    return <div className="RequestsInfo" style={{position: 'relative', height: '100%'}}>
      <h1>Requests</h1>
      {this.renderSelectedRequest({ topOffest })}
      <div ref={(ref) => { this.containerRef = ref }} className="table-container" style={{position: 'absolute', bottom: 0, top: topOffest}}>
        <table>
          <tbody>
            {this.props.reqs.map(this.renderItem)}
          </tbody>
        </table>

        {/* add some space on the bottom, make it easier to read */}
        <div style={{height: '50%'}} />
      </div>
    </div>;
  }
}
RequestsInfo.propTypes = {
  reqs: PropTypes.array.isRequired,
  onItemSelect: PropTypes.func.isRequired,
  selectedReqInfo: PropTypes.any // null, 'loading' or actual request object
}



@inject("store") @observer
export default class InstancePage extends React.Component {
  constructor() {
    super();

    this.onSelectRequest = this.onSelectRequest.bind(this);

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

  onSelectRequest(id) {
    this.props.store.onSelectRequest(id);
  }

  renderGrid() {
    return null;

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

  renderElement(C, e) {
    const { id, x1, x2, y1, y2, attrs } = e;
    const props = { id, x1, x2, y1, y2, attrs };
    return <C key={id} grid={grid} {...props} />;
  }

  render() {
    const reqs = this.props.store.layout.HttpReq || [];

    // for some reason setting viewportHeight for div height creates scrollbar
    // make it disappear using overflow: hidden
    return <div className="InstancePage" style={{height: this.state.viewportHeight, overflow: 'hidden'}}>
      <div className="map-container">
        <SvgView padding={100} paddingLeft={100} paddedWidth={300} paddedHeight={1000}>
          {this.renderGrid()}
          {reqs.map(this.renderElement.bind(this, HttpReq.Component))}
        </SvgView>
      </div>
      <div className="extra-info-container">
        <RequestsInfo reqs={this.props.store.httpRequestsList} selectedReqInfo={this.props.store.selectedReqInfo}
          onItemSelect={this.onSelectRequest} />
      </div>
    </div>;
  }
}
