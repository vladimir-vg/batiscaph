import React from 'react';
import { Link } from 'react-router-dom';
import PropTypes from 'prop-types';

import util from '../util';



export default class MarkedOutput extends React.Component {
  constructor() {
    super();

    this.hoverProcess = this.hoverProcess.bind(this);
  }

  hoverProcess(id) { this.props.store.hoverProcess(id); }

  render() {
    const { instanceId } = this.props;
    const { hoveredProcessPid, selectedProcessPid } = this.props.store;

    const nodes = [];
    util.eachToken(this.props.text, {
      onText: (text, i) => { nodes.push(<span key={i}>{text}</span>) },
      onPid: (pid, i) => {
        const path = `/instances/${instanceId}/process-info/${pid}`;
        let className = "pid-link";
        if (hoveredProcessPid === pid) {
          className += " hovered";
        }
        if (selectedProcessPid === pid) {
          className += " selected";
        }
        nodes.push(<Link key={i} to={path} className={className}
          onMouseEnter={this.hoverProcess.bind(this, pid)} 
          onMouseLeave={this.hoverProcess.bind(this, null)}>

          {pid}
        </Link>);
      },
    });

    let className = "MarkedOutput";
    if (this.props.className) {
      className += " " + this.props.className;
    }

    if (this.props.isBlock) {
      return <pre className={className}>{nodes}</pre>;
    } else {
      return <code className={className}>{nodes}</code>;
    }
  }
}
MarkedOutput.propTypes = {
  text: PropTypes.string.isRequired,
  instanceId: PropTypes.string.isRequired,
  store: PropTypes.object.isRequired,

  isBlock: PropTypes.bool,
  className: PropTypes.string,
};
MarkedOutput.defaultProps = {
  isBlock: true
};