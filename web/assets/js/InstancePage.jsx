import React from 'react';
import { Route, Link } from 'react-router-dom';
import PropTypes from 'prop-types';
import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import SvgView from './SvgView';
import RequestsListPage from './RequestsListPage';
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



// class Tabs extends React.Component {
//   constructor() {
//     super();
// 
//     this.renderItem = this.renderItem.bind(this);
//   }
// 
//   renderItem({ id, text }) {
//     let className = "item";
//     if (id === this.props.selectedId) {
//       className += " active";
//     }
// 
//     return <span key={id} className={className} onClick={this.props.onSelect.bind(this, id)}>
//       {text}
//     </span>;
//   }
// 
//   render() {
//     return <div className="Tabs">
//       {this.props.choices.map(this.renderItem)}
//     </div>;
//   }
// }
// Tabs.propTypes = {
//   selectedId: PropTypes.string.isRequired,
//   choices: PropTypes.array.isRequired,
//   onSelect: PropTypes.func.isRequired,
// }



@inject("store") @observer
export default class InstancePage extends React.Component {
  constructor() {
    super();

    this.onRequestSelect = this.onRequestSelect.bind(this);
    this.onRequestHover = this.onRequestHover.bind(this);
    this.selectProcess = this.selectProcess.bind(this);
    this.onTabSelect = this.onTabSelect.bind(this);
    this.renderElement = this.renderElement.bind(this);

    // TODO: listen to resize event, update height accordingly
    const viewportHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
    this.state = {
      viewportHeight,
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

  selectProcess(pid) {
    const { id } = this.props.match.params;
    const newPath = `/instances/${id}/process_info/${encodeURIComponent(pid)}`;
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
    const { onRequestSelect, onRequestHover, selectProcess } = this; // take wrapped functions
    const storeProps = { onRequestSelect, onRequestHover, selectedRequestId, hoveredRequestId, selectProcess };
    const { id, key, Component, ...elementProps} = e;
    return <Component key={key} grid={grid} {...storeProps} id={id} {...elementProps} />;
  }

  render() {
    const reqs = this.props.store.layout.HttpReq || [];
    const procs = this.props.store.layout.Process || [];

    // Display process link, if it is currently selected
    let processLink = null;
    if (/\/process_info\//.test(this.props.location.pathname)) {
      processLink = <Link to={this.props.location.pathname} activeClassName="active">Process</Link>
    }

    // for some reason setting viewportHeight for div height creates scrollbar
    // make it disappear using overflow: hidden
    return <div className="InstancePage" style={{height: this.state.viewportHeight, overflow: 'hidden'}}>
      <div className="extra-info-container">
        <div className="Tabs">
          <Link to={`${this.props.match.url}/requests`} activeClassName="active">Requests</Link>
          <Link to={`${this.props.match.url}/shell`} activeClassName="active">Shell</Link>
          {processLink}
        </div>

        <Route exact path={`${this.props.match.path}/requests`} component={RequestsListPage} />
        <Route exact path={`${this.props.match.path}/shell`} component={ShellPanelPage} />
        <Route exact path={`${this.props.match.path}/process_info/:pid`} component={ProcessPanelPage} />
      </div>
      <div className="map-container">
        <SvgView padding={100} paddingLeft={100} paddedWidth={300} paddedHeight={1000}>
          {this.renderGrid()}
          <g>{procs.map(this.renderElement)}</g>
          <g>{reqs.map(this.renderElement)}</g>
        </SvgView>
      </div>
    </div>;
  }
}
