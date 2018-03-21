import React from 'react';
import PropTypes from 'prop-types';



class SelectedRequestInfo extends React.Component {
  renderHeaderItem([key, value], i) {
    const key1 = key
      .replace(/^([a-z])/, (a, l) => l.toUpperCase())
      .replace(/-([a-z])/, (a, l) => '-' + l.toUpperCase())
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

    const { method, path, resp_code, resp_headers, req_headers } = this.props.selectedReqInfo;

    return <div className="SelectedRequestInfo" style={{position: 'absolute', bottom: 0, top: 0, backgroundColor: 'white', zIndex: 1, width: '100%'}}>
      <div>
        <div style={{display: 'flex'}}>
          <div style={{flex: '1'}}>{method} {path} {resp_code}</div>
          <div style={{flex: '0'}}><button onClick={this.props.clearSelection}>×</button></div>
        </div>

        <h2>request headers:</h2>
        {this.renderHeaders(req_headers)}

        <h2>response headers:</h2>
        {this.renderHeaders(resp_headers)}
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