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

  renderRects(currentOffset) {
    if (!this.state.svgWidth) { return null; }

    const parts = producePlugParts(this.props.selectedReqInfo.Plugs, this.state.durationScale, 0.5, this.props.barHeight);
    const nodes = [];
    for (let i in parts) {
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
      duration = this.state.hoveredPlug.duration;
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



class SelectedRequestInfo extends React.Component {
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
    if (!this.props.selectedReqInfo) { return null; }
    if (this.props.selectedReqInfo === 'loading') {
      return <div style={{position: 'absolute', bottom: 0, top: 0, backgroundColor: 'white', zIndex: 1, width: '100%'}}>
        loading...
        <button onClick={this.props.clearSelection}>×</button>
      </div>;
    }

    const { Method, Path, RespCode, RespHeaders, ReqHeaders } = this.props.selectedReqInfo;

    let plugInfo = null;
    if (this.props.selectedReqInfo.Plugs) {
      plugInfo = <PlugsInfo barHeight={20} selectedReqInfo={this.props.selectedReqInfo} />;
    }

    return <div className="SelectedRequestInfo" style={{position: 'absolute', bottom: 0, top: 0, backgroundColor: 'white', zIndex: 1, width: '100%'}}>
      <div>
        <div style={{display: 'flex'}}>
          <div style={{flex: '1'}}>{Method} {Path} {RespCode}</div>
          <div style={{flex: '0'}}><button onClick={this.props.clearSelection}>×</button></div>
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
SelectedRequestInfo.propTypes = {
  clearSelection: PropTypes.func.isRequired,
  selectedReqInfo: PropTypes.any // null, 'loading' or actual request object
}



export default class RequestsList extends React.Component {
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
      const { scrollTop } = this.containerRef;

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

  renderItem({ Id, Method, Path, RespCode }) {
    let className = "";
    if (Id === this.props.hoveredRequestId) {
      className += " hovered";
    }

    return <tr key={Id} className={className}
        onClick={this.onRequestSelect.bind(this, Id)}
        onMouseEnter={this.onRequestHover.bind(this, Id)}
        onMouseLeave={this.onRequestHover.bind(this, null)}>

      <td>{Method}</td>
      <td>{Path}</td>
      <td>{RespCode}</td>
    </tr>;
  }

  render() {
    const topOffset = 90;
    return <div className="RequestsInfo" style={{position: 'relative', height: '100%'}}>
      <h1>Requests</h1>
      <SelectedRequestInfo
        selectedReqInfo={this.props.selectedReqInfo}
        clearSelection={this.onRequestSelect.bind(this, null)} />
      <div ref={(ref) => { this.containerRef = ref }} className="table-container" style={{position: 'absolute', bottom: 0, top: topOffset}}>
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
RequestsList.propTypes = {
  reqs: PropTypes.array.isRequired,
  onRequestSelect: PropTypes.func.isRequired,
  onRequestHover: PropTypes.func.isRequired,
  selectedRequestId: PropTypes.string,
  hoveredRequestId: PropTypes.string,
  selectedReqInfo: PropTypes.any // null, 'loading' or actual request object
}