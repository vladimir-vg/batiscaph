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



class SelectedRequestInfo extends React.Component {
  renderHeaderItem([key, value], i) {
    return <div key={i}>{key}: {value}</div>;
  }

  renderHeaders(headers) {
    return <code>
      {headers.map(this.renderHeaderItem)}
    </code>;
  }

  render() {
    if (!this.props.selectedReqInfo) { return null; }

    if (this.props.selectedReqInfo === 'loading') {
      return <div style={{position: 'absolute', bottom: 0, top: this.props.topOffest, backgroundColor: 'white', zIndex: 1, width: '100%'}}>
        loading...
        <button onClick={this.props.clearSelection}>x</button>
      </div>;
    }

    const { method, path, resp_code, resp_headers, req_headers } = this.props.selectedReqInfo;

    return <div style={{position: 'absolute', bottom: 0, top: this.props.topOffest, backgroundColor: 'white', zIndex: 1, width: '100%'}}>
      <div style={{display: 'flex'}}>
        <div style={{flex: '1'}}>{method} {path} {resp_code}</div>
        <div style={{flex: '0'}}><button onClick={this.props.clearSelection}>x</button></div>
      </div>

      <h2>request headers:</h2>
      {this.renderHeaders(req_headers)}

      <h2>response headers:</h2>
      {this.renderHeaders(resp_headers)}
    </div>;
  }
}
SelectedRequestInfo.propTypes = {
  topOffest: PropTypes.number.isRequired,
  clearSelection: PropTypes.func.isRequired,
  selectedReqInfo: PropTypes.any // null, 'loading' or actual request object
}



class RequestsInfo extends React.Component {
  constructor() {
    super();

    // only affects scroll, shouldn't be put into state
    // not part of the renderable state
    this.stickToBottom = true;

    this.renderItem = this.renderItem.bind(this);
    this.onRequestSelect = this.onRequestSelect.bind(this);
    this.onRequestHover = this.onRequestHover.bind(this);
  }

  componentDidMount() {
    this.containerRef.addEventListener("scroll", () => {
      const fullScroll = this.containerRef.scrollHeight - this.containerRef.clientHeight;
      const scrollTop = this.containerRef.scrollTop;

      // stick to bottom, if we scrolled to very bottom
      // if was scrolled up, then unstick
      this.stickToBottom = (scrollTop === fullScroll);
    });
  }

  componentDidUpdate() {
    if (this.stickToBottom) {
      this.containerRef.scrollTop = this.containerRef.scrollHeight - this.containerRef.clientHeight;
    }
  }

  onRequestSelect(id) { this.props.onRequestSelect(id); }
  onRequestHover(id) { this.props.onRequestHover(id); }

  renderItem({ id, method, path, resp_code }) {
    let className = "";
    if (id === this.props.hoveredRequestId) {
      className += " hovered";
    }

    return <tr key={id} className={className}
        onClick={this.onRequestSelect.bind(this, id)}
        onMouseEnter={this.onRequestHover.bind(this, id)}
        onMouseLeave={this.onRequestHover.bind(this, null)}>

      <td>{method}</td>
      <td>{path}</td>
      <td>{resp_code}</td>
    </tr>;
  }

  render() {
    const topOffest = 60;
    return <div className="RequestsInfo" style={{position: 'relative', height: '100%'}}>
      <h1>Requests</h1>
      <SelectedRequestInfo topOffest={topOffest}
        selectedReqInfo={this.props.selectedReqInfo}
        clearSelection={this.onRequestSelect.bind(this, null)} />
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
  onRequestSelect: PropTypes.func.isRequired,
  onRequestHover: PropTypes.func.isRequired,
  selectedRequestId: PropTypes.string,
  hoveredRequestId: PropTypes.string,
  selectedReqInfo: PropTypes.any // null, 'loading' or actual request object
}



@inject("store") @observer
export default class InstancePage extends React.Component {
  constructor() {
    super();

    this.onRequestSelect = this.onRequestSelect.bind(this);
    this.onRequestHover = this.onRequestHover.bind(this);

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
    const { selectedRequestId, hoveredRequestId } = this.props.store;
    const { onRequestSelect, onRequestHover } = this; // take wrapped functions
    const storeProps = { onRequestSelect, onRequestHover, selectedRequestId, hoveredRequestId };
    const { id, x1, x2, y1, y2, attrs } = e;
    const layoutProps = { id, x1, x2, y1, y2, attrs };
    return <C key={id} grid={grid} {...storeProps} {...layoutProps} />;
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
        <RequestsInfo
          reqs={this.props.store.httpRequestsList} selectedReqInfo={this.props.store.selectedReqInfo}
          selectedRequestId={this.props.store.selectedRequestId} hoveredRequestId={this.props.store.hoveredRequestId}
          onRequestSelect={this.onRequestSelect} onRequestHover={this.onRequestHover} />
      </div>
    </div>;
  }
}
