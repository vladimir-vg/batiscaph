import React from 'react';
import PropTypes from 'prop-types';

import c from '../constraint';



const PROC_WIDTH = 6;
const EXIT_HEIGHT = 2;
class Component extends React.Component {
  constructor() {
    super();

    this.onClick = this.onClick.bind(this);
  }

  onClick() {
    this.props.selectProcess(this.props.id);
  }

  render() {
    const g = this.props.grid;
    const sideOffset = (g.xColWidth - PROC_WIDTH)/2;
    const x = g.xColStart(this.props.x) + sideOffset;
    const width = PROC_WIDTH;
    const y = g.yRowAt(this.props.startedY);
    const height = (g.yRowAt(this.props.continueY || this.props.exitedY) - g.yRowAt(this.props.startedY));

    let exit = null;
    if (this.props.exitedY) {
      const y = g.yRowAt(this.props.exitedY)-EXIT_HEIGHT/2;
      exit = <rect className="exit" x={x} y={y} width={width} height={EXIT_HEIGHT} />
    }

    let spawnLines = null;
    if (typeof this.props.parentX === 'number') { // might be zero
      let parentX = Math.floor(g.xColWidth/2) + g.xColStart(this.props.parentX);
      let parentY1 = y - g.yRowHeight;
      spawnLines = <g>
        <line className="spawn" x1={x} y1={y+0.5} x2={parentX} y2={y+0.5} />
        <line className="spawn" x1={parentX+0.5} y1={y+0.5} x2={parentX+0.5} y2={parentY1+0.5} />
      </g>;
    }

    return <g className="Process">
      <rect onClick={this.onClick} className="body" x={x} y={y} width={width} height={height} />
      {exit}
      {spawnLines}
    </g>;
  }
}
Component.propTypes = {
  id: PropTypes.string.isRequired,
  x: PropTypes.number.isRequired,
  startedY: PropTypes.number.isRequired,
  exitedY: PropTypes.number,
  continueY: PropTypes.number,
  parentX: PropTypes.number,

  selectProcess: PropTypes.func.isRequired,
}



function produceElements(delta) {
  const result = [];

  delta['erlang-processes'].forEach((proc, pid) => {
    // skip
    if (!proc.SpawnedAt && !proc.TraceStartedAt) { return; }

    const constraints = {
      x: c.xPid(proc.Pid),
      startedY: c.yTimestamp(proc.SpawnedAt || proc.TraceStartedAt),
    }
    if (proc.SpawnedAt) {
      constraints.parentX = c.xPid(proc.ParentPid);
    }

    if (proc.ExitedAt) {
      constraints.exitedY = c.yTimestamp(proc.ExitedAt);
    } else {
      constraints.continueY = c.yTimestampNow();
    }

    result.push({ id: pid, key: pid, Component, constraints });
  });

  return result;
};


export default {
  produceElements // , Component
};