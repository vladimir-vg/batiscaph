import React from 'react';
import { Link } from 'react-router-dom';
import PropTypes from 'prop-types';

import util from '../util';



export default class MarkedOutput extends React.Component {
  constructor() {
    super();

    this.hoverProcess = this.hoverProcess.bind(this);
  }

  hoverProcess(id) { this.props.hoverProcess(id); }

  render() {
    const { instanceId } = this.props;
    const nodes = [];
    util.eachToken(this.props.text, {
      onText: (text, i) => { nodes.push(<span key={i}>{text}</span>) },
      onPid: (pid, i) => {
        const path = `/instances/${instanceId}/process-info/${pid}`;
        let className = "pid-link";
        if (this.props.hoveredProcessPid === pid) {
          className += " hovered";
        }
        nodes.push(<Link key={i} to={path} className={className}
          onMouseEnter={this.hoverProcess.bind(this, pid)} 
          onMouseLeave={this.hoverProcess.bind(this, null)}>

          {pid}
        </Link>);
      },
    });
    return <pre className="MarkedOutput">{nodes}</pre>;
  }
}
MarkedOutput.propTypes = {
  hoverProcess: PropTypes.func.isRequired,
  hoveredProcessPid: PropTypes.string,
  text: PropTypes.string.isRequired,
  instanceId: PropTypes.string.isRequired,
};