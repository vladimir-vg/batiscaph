import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';



@inject("store") @observer
export default class RequestsListPage extends React.Component {
  constructor() {
    super();

    this.renderItem = this.renderItem.bind(this);
    this.selectCallback = this.selectCallback.bind(this);
    this.hoverCallback = this.hoverCallback.bind(this);
  }

  componentWillUnmount() {
    this.props.store.hoverCallback(null);
  }

  hoverCallback(id) { this.props.store.hoverCallback(id); }

  selectCallback(reqId) {
    const { id } = this.props.match.params;
    const newPath = `/instances/${id}/request-info/${reqId}`;
    this.props.history.push(newPath);
  }

  renderItem({ Id1, Method, Path, RespCode }) {
    let className = "";
    if (Id1 === this.props.store.hoveredCallbackId) {
      className += " hovered";
    }

    return <tr key={Id1} className={className}
        onClick={this.selectCallback.bind(this, Id1)}
        onMouseEnter={this.hoverCallback.bind(this, Id1)}
        onMouseLeave={this.hoverCallback.bind(this, null)}>

      <td>{Method}</td>
      <td>{Path}</td>
      <td>{RespCode}</td>
    </tr>;
  }

  render() {
    return <div className="RequestsListPage">
        <table>
          <tbody>
            {this.props.store.httpRequestsList.map(this.renderItem)}
          </tbody>
        </table>

        {/* add some space on the bottom, make it easier to read */}
        <div style={{height: 300}} />
    </div>;
  }
}
