import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';
import PropTypes from 'prop-types';
import { scaleLinear } from 'd3-scale';



@inject("store") @observer
export default class RequestsListPage extends React.Component {
  constructor() {
    super();

    // only affects scroll, shouldn't be put into state
    // not part of the renderable state
    this.stickToBottom = true;

    this.renderItem = this.renderItem.bind(this);
    this.selectRequest = this.selectRequest.bind(this);
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

  onRequestHover(id) { this.props.store.onRequestHover(id); }

  selectRequest(reqId, type) {
    const { id } = this.props.match.params;
    const newPath = `/instances/${id}/${type}-request-info/${reqId}`;
    this.props.history.push(newPath);
  }

  renderItem({ Id, Method, Path, RespCode, _type }) {
    let className = "";
    if (Id === this.props.store.hoveredRequestId) {
      className += " hovered";
    }

    return <tr key={Id} className={className}
        onClick={this.selectRequest.bind(this, Id, _type)}
        onMouseEnter={this.onRequestHover.bind(this, Id)}
        onMouseLeave={this.onRequestHover.bind(this, null)}>

      <td>{Method}</td>
      <td>{Path}</td>
      <td>{RespCode}</td>
    </tr>;
  }

  render() {
    return <div className="RequestsInfo">
      <div ref={(ref) => { this.containerRef = ref }} className="table-container">
        <table>
          <tbody>
            {this.props.store.httpRequestsList.map(this.renderItem)}
          </tbody>
        </table>

        {/* add some space on the bottom, make it easier to read */}
        <div style={{height: '50%'}} />
      </div>
    </div>;
  }
}
