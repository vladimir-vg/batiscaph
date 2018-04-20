import React from 'react';
import { Route, NavLink } from 'react-router-dom';
import PropTypes from 'prop-types';
import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import SvgView from './SvgView';
import RequestsListPage from './RequestsListPage';
import RequestPage from './RequestPage';
import ShellPanelPage from './ShellPanelPage';
import ProcessPanelPage from './ProcessPanelPage';



const hGap = 4;
const colWidth = 12;
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

    this.selectRequest = this.selectRequest.bind(this);
    this.onRequestHover = this.onRequestHover.bind(this);
    this.selectProcess = this.selectProcess.bind(this);
    this.onTabSelect = this.onTabSelect.bind(this);
    this.renderElement = this.renderElement.bind(this);

    // only affects scroll, shouldn't be put into state
    // not part of the renderable state
    this.wasScrolledToBottom = true;

    // TODO: listen to resize event, update height accordingly
    const viewportHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
    this.state = {
      viewportHeight,
    };
  }

  componentWillMount() {
    this.props.store.subscribeToInstance(this.props.match.params.id);
  }


  // keep scrolled down, even if new content is added
  // if was scrolled down before
  componentDidMount() {
    this.containerRef.addEventListener("scroll", () => {
      const fullScroll = this.containerRef.scrollHeight - this.containerRef.clientHeight;
      const { scrollTop } = this.containerRef;
    
      // stick to bottom, if we scrolled to very bottom
      // if was scrolled up, then unstick
      this.wasScrolledToBottom = (scrollTop === fullScroll);
    });
  }

  componentDidUpdate() {
    if (this.wasScrolledToBottom && this.shouldStickToBottom()) {
      this.containerRef.scrollTop = this.containerRef.scrollHeight - this.containerRef.clientHeight;
    }
  }



  componentWillUnmount() {
    this.props.store.unsubscribeFromInstance(this.props.match.params.id);
  }

  shouldStickToBottom() {
    const { pathname } = this.props.location;
    if (/\/requests$/.test(pathname)) { return true; }
    return false;
  }

  // onRequestSelect(id) { this.props.store.onRequestSelect(id); }
  onRequestHover(id) { this.props.store.onRequestHover(id); }

  selectRequest(reqId, type) {
    const { id } = this.props.match.params;
    const newPath = `/instances/${id}/${type}-request-info/${encodeURIComponent(reqId)}`;
    this.props.history.push(newPath);
  }

  selectProcess(pid) {
    const { id } = this.props.match.params;
    const newPath = `/instances/${id}/process-info/${encodeURIComponent(pid)}`;
    this.props.history.push(newPath);
  }

  onTabSelect(tabId) {
    this.setState({ tabId });
  }

  renderGrid() {
    if (!this.props.store.gridEnabled) { return null; }

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
    const { selectRequest, onRequestHover, selectProcess } = this; // take wrapped functions
    const storeProps = { selectRequest, onRequestHover, selectedRequestId, hoveredRequestId, selectProcess };
    const { id, key, Component, ...elementProps} = e;
    return <Component key={key} grid={grid} {...storeProps} id={id} {...elementProps} />;
  }

  render() {
    const reqs = this.props.store.layout.HttpReq || [];
    const procs = this.props.store.layout.Process || [];

    const usedWidth = grid.xColStart(this.props.store.layout.xColsLength+1);
    const usedHeight = grid.yRowAt(this.props.store.layout.yRowsLength);

    // Display process link, if it is currently selected
    let processLink = null;
    if (/\/process_info\//.test(this.props.location.pathname)) {
      processLink = <NavLink to={this.props.location.pathname}>Process</NavLink>
    }

    // for some reason setting viewportHeight for div height creates scrollbar
    // make it disappear using overflow: hidden
    return <div className="InstancePage" style={{height: this.state.viewportHeight, overflow: 'hidden'}}>
      <div className="extra-info-container">
        <div className="Tabs">
          <NavLink to={`${this.props.match.url}/requests`}>Requests</NavLink>
          <NavLink to={`${this.props.match.url}/shell`}>Shell</NavLink>
          {processLink}
        </div>

        <div className="tab-container" ref={(ref) => { this.containerRef = ref }}>
          <Route exact path={`${this.props.match.path}/requests`} component={RequestsListPage} />
          <Route exact path={`${this.props.match.path}/shell`} component={ShellPanelPage} />
          <Route exact path={`${this.props.match.path}/process-info/:pid`} component={ProcessPanelPage} />
          <Route exact path={`${this.props.match.path}/plug-request-info/:reqId`} component={RequestPage} />
          <Route exact path={`${this.props.match.path}/cowboy-request-info/:reqId`} component={RequestPage} />
        </div>
      </div>
      <div className="map-container">
        <SvgView padding={100} paddingLeft={100} paddedWidth={usedWidth} paddedHeight={usedHeight}>
          {this.renderGrid()}
          <g>{procs.map(this.renderElement)}</g>
          <g>{reqs.map(this.renderElement)}</g>
        </SvgView>
      </div>
    </div>;
  }
}
