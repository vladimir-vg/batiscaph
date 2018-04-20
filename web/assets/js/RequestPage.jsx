import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';
import PropTypes from 'prop-types';
import { scaleLinear } from 'd3-scale';



const producePlugParts = (plugs, scale, offset, height) => {
  let result = [];
  offset = offset || 0.5;
  plugs.forEach((p) => {
    const width = scale(p.StoppedAt - p.StartedAt);
    const width1 = Math.max(Math.floor(width), 2)
    result.push({
      x: offset, width: width1, height: height,
      module: p.Module, duration: p.StoppedAt - p.StartedAt
    });

    if (p.Plugs) {
      const items = producePlugParts(p.Plugs, scale, offset, height-2);
      result = result.concat(items);
    }

    offset += width1;
  })

  return result;
};



const formatDuration = (mcs) => {
  if (mcs < 1000) { return '0.' + mcs.toString().padStart(3, "0") + 'ms' }
  return '' + Math.floor(mcs / 1000) + 'ms';
};

const formatPlugModule = (module) => {
  return module.replace(/^Elixir./, '');
};



class PlugsInfo extends React.Component {
  constructor() {
    super();

    this.state = {
      svgWidth: null,
      durationScale: null,
      hoveredPlug: null, // object
    };
  }

  componentDidMount() {
    // TODO: listen to resize, update width
    // should draw plugs in such a way that every plug will be visible
    // no matter how short its duration was,
    // yet try to display all plugs proportionally to taken time
    const { StartedAt, StoppedAt } = this.props.selectedReqInfo;
    const totalDurationMcs = StoppedAt - StartedAt;
    const durationScale = scaleLinear()
      .domain([0, totalDurationMcs])
      .range([0, this.svgRef.clientWidth]);
    this.setState({svgWidth: this.svgRef.clientWidth, durationScale });
  }

  onPlugHover(plug) {
    this.setState({hoveredPlug: plug});
  }

  renderRects() {
    if (!this.state.svgWidth) { return null; }

    const parts = producePlugParts(this.props.selectedReqInfo.Plugs, this.state.durationScale, 0.5, this.props.barHeight);
    const nodes = [];
    for (const i in parts) {
      const p = parts[i];

      let className = "plug";
      // let height = this.props.barHeight;
      if (this.state.hoveredPlug && this.state.hoveredPlug.module === p.module) {
        className += " hovered";
      }

      nodes.push(<rect key={i}
        className={className}
        onMouseEnter={this.onPlugHover.bind(this, p)}
        onMouseLeave={this.onPlugHover.bind(this, null)}
        x={p.x} y={0.5} width={p.width} height={p.height-0.5*2} />);
    }

    return <g>{nodes}</g>;
  }

  render() {
    let moduleName = '';
    const { StartedAt, StoppedAt } = this.props.selectedReqInfo;
    let duration = StoppedAt - StartedAt;

    if (this.state.hoveredPlug) {
      moduleName = formatPlugModule(this.state.hoveredPlug.module);
      ({ duration } = this.state.hoveredPlug.duration);
    }

    return <div>
      <svg ref={(ref) => { this.svgRef = ref; }}
          height={this.props.barHeight} width="100%">
        {this.renderRects()}
      </svg>
      <div>{formatDuration(duration)} {moduleName}</div>
    </div>;
  }
}
PlugsInfo.propTypes = {
  selectedReqInfo: PropTypes.object.isRequired,
  barHeight: PropTypes.number.isRequired,
}



@inject("store") @observer
export default class RequestPage extends React.Component {
  constructor() {
    super();
    this.clearSelection = this.clearSelection.bind(this);
  }

  componentWillMount() {
    const { reqId } = this.props.match.params;
    this.props.store.onRequestSelect(reqId, this.getRequestType());
  }

  componentWillReceiveProps(props) {
    if (props.match.params.reqId !== this.props.match.params.reqId) {
      this.props.store.onRequestSelect(props.match.params.reqId, this.getRequestType(props));
    }
  }

  componentWillUnmount() {
    this.props.store.onRequestSelect(null);
  }

  getRequestType(props) {
    props = props || this.props;

    const { path } = props.match;
    if (/plug-request-info/.test(path)) { return 'plug'; }
    if (/cowboy-request-info/.test(path)) { return 'cowboy'; }

    throw {
      message: "Unknown request type path",
      path
    };
  }

  clearSelection() {
    const { id } = this.props.match.params;
    const newPath = `/instances/${id}/requests`;
    this.props.history.push(newPath);
  }

  renderHeaderItem([key, value], i) {
    const key1 = key
      .replace(/^([a-z])/, (a, l) => l.toUpperCase())
      .replace(/-([a-z])/g, (a, l) => '-' + l.toUpperCase())
    return <div key={i}>{key1}:&nbsp;<span className="value">{value}</span></div>;
  }

  renderHeaders(headers) {
    return <code>
      {headers.map(this.renderHeaderItem)}
    </code>;
  }

  render() {
    if (!this.props.store.selectedReqInfo) { return null; }
    if (this.props.store.selectedReqInfo === 'loading') {
      return <div style={{position: 'absolute', bottom: 0, top: 0, backgroundColor: 'white', zIndex: 1, width: '100%'}}>
        loading...
        <button onClick={this.clearSelection}>×</button>
      </div>;
    }

    const { Method, Path, RespCode, RespHeaders, ReqHeaders } = this.props.store.selectedReqInfo;

    let plugInfo = null;
    if (this.props.store.selectedReqInfo.Plugs) {
      plugInfo = <PlugsInfo barHeight={20} selectedReqInfo={this.props.store.selectedReqInfo} />;
    }

    return <div className="SelectedRequestInfo" style={{position: 'absolute', bottom: 0, top: 0, backgroundColor: 'white', zIndex: 1, width: '100%'}}>
      <div>
        <div style={{display: 'flex'}}>
          <div style={{flex: '1'}}>{Method} {Path} {RespCode}</div>
          <div style={{flex: '0'}}><button onClick={this.clearSelection}>×</button></div>
        </div>

        {plugInfo}

        <h2>request headers:</h2>
        {this.renderHeaders(ReqHeaders)}

        <h2>response headers:</h2>
        {this.renderHeaders(RespHeaders)}
      </div>
    </div>;
  }
}
// RequestPage.propTypes = {
//   clearSelection: PropTypes.func.isRequired,
//   selectedReqInfo: PropTypes.any // null, 'loading' or actual request object
// }