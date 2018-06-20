import React from 'react';
import { Route, NavLink, Switch, Redirect } from 'react-router-dom';
import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import Layout from '../components/svgLayout';
import SvgView from '../components/SvgView';

import RequestsListPage from './RequestsListPage';
import RequestPage from './RequestPage';
import ShellPage from './ShellPage';
import LogsPage from './LogsPage';
import ProcessPage from './ProcessPage';



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

    this.selectCallback = this.selectCallback.bind(this);
    this.hoverCallback = this.hoverCallback.bind(this);
    this.selectProcess = this.selectProcess.bind(this);
    this.hoverProcess = this.hoverProcess.bind(this);
    this.hoverLogEvent = this.hoverLogEvent.bind(this);
    this.onTabSelect = this.onTabSelect.bind(this);
    this.renderElement = this.renderElement.bind(this);

    // only affects scroll, shouldn't be put into state
    // not part of the renderable state
    this.wasScrolledToBottom = true;

    // TODO: listen to resize event, update height accordingly
    const viewportHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
    this.state = {
      viewportHeight,
      layoutRefs: {
        selectedItemBackground: React.createRef(),
        callbackRects: React.createRef(),
        callbackActiveRects: React.createRef(),
        procBody: React.createRef(),
        procSpawnLines: React.createRef(),
        procSpawnLinesOverBody: React.createRef(),
        logEvents: React.createRef(),
      }
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
    if (/\/shell$/.test(pathname)) { return true; }
    return false;
  }

  hoverCallback(id) { this.props.store.hoverCallback(id); }
  hoverProcess(id) { this.props.store.hoverProcess(id); }
  hoverLogEvent(id) { this.props.store.hoverLogEvent(id); }

  selectCallback(reqId) {
    const { id } = this.props.match.params;
    if (!reqId) {
      const newPath = `/instances/${id}/requests`;
      this.props.history.push(newPath);
    } else {
      const newPath = `/instances/${id}/request-info/${encodeURIComponent(reqId)}`;
      this.props.history.push(newPath);
    }
  }

  selectProcess(pid) {
    const { id } = this.props.match.params;
    if (!pid) {
      const newPath = `/instances/${id}`;
      this.props.history.push(newPath);
    } else {
      const newPath = `/instances/${id}/process-info/${encodeURIComponent(pid)}`;
      this.props.history.push(newPath);
    }
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

  renderSelection() {
    const result = [];
    if (this.props.store.hoveredLogId) {
      const element = this.props.store.layout.LogEvent[this.props.store.hoveredLogId];
      if (element) { result.push(this.renderSelectionElement(element)); }
    }
    if (this.props.store.hoveredCallbackId) {
      const element = this.props.store.layout.Callback[this.props.store.hoveredCallbackId];
      if (element) { result.push(this.renderSelectionElement(element, "hovered")); }
    }
    if (this.props.store.selectedCallbackId) {
      const element = this.props.store.layout.Callback[this.props.store.selectedCallbackId];
      if (element) { result.push(this.renderSelectionElement(element, "selected")); }
    }
    if (this.props.store.hoveredProcessPid) {
      const element = this.props.store.layout.Process[this.props.store.hoveredProcessPid];
      if (element) { result.push(this.renderSelectionElement(element, "hovered")); }
    }
    if (this.props.store.selectedProcessPid) {
      const element = this.props.store.layout.Process[this.props.store.selectedProcessPid];
      if (element) { result.push(this.renderSelectionElement(element, "selected")); }
    }
    return result;
  }

  renderSelectionElement(element, keyPrefix) {
    const { key, Component, SelectionBackgroundComponent, ...elementProps } = element;
    const key1 = `${keyPrefix || ''} ${key}`;
    return <SelectionBackgroundComponent key={key1} grid={grid} {...elementProps} />;
  }

  renderElement([_key, e]) {
    const { selectedCallbackId, hoveredCallbackId, selectedProcessPid, hoveredProcessPid, hoveredLogId } = this.props.store;
    const { selectCallback, hoverCallback, selectProcess, hoverProcess, hoverLogEvent } = this; // take wrapped functions
    const storeProps = {
      selectCallback, selectProcess,
      hoverCallback, hoverProcess,
      selectedCallbackId, hoveredCallbackId,
      selectedProcessPid, hoveredProcessPid,
      hoveredLogId, hoverLogEvent
    };

    const { id, key, Component, ...elementProps} = e;
    return <Component key={key} grid={grid} {...storeProps} id={id} {...elementProps} />;
  }

  render() {
    const callbacks = this.props.store.layout.Callback || {};
    const procs = this.props.store.layout.Process || {};
    const logs = this.props.store.layout.LogEvent || {};

    const usedWidth = grid.xColStart(this.props.store.layout.xColsLength+1);
    const usedHeight = grid.yRowAt(this.props.store.layout.yRowsLength);

    // Display process link, if it is currently selected
    let selectedItemLink = null;
    if (/\/process-info\//.test(this.props.location.pathname)) {
      selectedItemLink = <NavLink to={this.props.location.pathname}>Process</NavLink>
    } else if (/\/\/request-info\//.test(this.props.location.pathname)) {
      selectedItemLink = <NavLink to={this.props.location.pathname}>Request</NavLink>
    }

    let requestsLink = null;
    if (Object.keys(callbacks).length != 0) {
      requestsLink = <NavLink to={`${this.props.match.url}/requests`}>Requests</NavLink>;
    }

    let logsLink = null;
    if (Object.keys(logs).length != 0) {
      logsLink = <NavLink to={`${this.props.match.url}/logs`}>Logs</NavLink>;
    }

    // for some reason setting viewportHeight for div height creates scrollbar
    // make it disappear using overflow: hidden
    return <div className="InstancePage" style={{height: this.state.viewportHeight, overflow: 'hidden'}}>
      <div className="extra-info-container">
        <div className="Tabs">
          <NavLink to={`${this.props.match.url}/shell`}>Shell</NavLink>
          {requestsLink}
          {logsLink}
          {selectedItemLink}
        </div>

        <div className="tab-container" ref={(ref) => { this.containerRef = ref }}>
          <Switch>
            <Route exact path={`${this.props.match.path}/requests`} component={RequestsListPage} />
            <Route exact path={`${this.props.match.path}/shell`} component={ShellPage} />
            <Route exact path={`${this.props.match.path}/logs`} component={LogsPage} />
            <Route exact path={`${this.props.match.path}/process-info/:pid`} component={ProcessPage} />
            <Route exact path={`${this.props.match.path}/request-info/:reqId`} component={RequestPage} />
            <Redirect to={`/instances/${this.props.match.params.id}/shell`} />
          </Switch>
        </div>
      </div>
      <div className="map-container">
        <SvgView padding={100} paddingLeft={100} paddedWidth={usedWidth} paddedHeight={usedHeight}>
          {this.renderGrid()}

          <Layout.Provider value={this.state.layoutRefs}>
            <g>{this.renderSelection()}</g>
            <g>{Object.entries(procs).map(this.renderElement)}</g>
            <g>{Object.entries(callbacks).map(this.renderElement)}</g>
            <g>{Object.entries(logs).map(this.renderElement)}</g>
          </Layout.Provider>

          <g ref={this.state.layoutRefs.selectedItemBackground} />
          <g ref={this.state.layoutRefs.callbackRects} />
          <g ref={this.state.layoutRefs.procSpawnLines} />
          <g ref={this.state.layoutRefs.procBody} />
          <g ref={this.state.layoutRefs.logEvents} />
          <g ref={this.state.layoutRefs.procSpawnLinesOverBody} />
          <g ref={this.state.layoutRefs.callbackActiveRects} />
        </SvgView>
      </div>
    </div>;
  }
}
