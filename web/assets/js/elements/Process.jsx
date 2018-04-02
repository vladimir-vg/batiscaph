import React from 'react';
import PropTypes from 'prop-types';

import c from '../constraint';


const PROC_WIDTH = 4;

class Component extends React.Component {
  render() {
    const g = this.props.grid;
    const sideOffset = (g.xColWidth - PROC_WIDTH)/2;
    const x = g.xColStart(this.props.x) + sideOffset;
    const width = PROC_WIDTH;
    const y = g.yRowAt(this.props.startedY);
    const height = (g.yRowAt(this.props.continueY || this.props.exitedY) - g.yRowAt(this.props.startedY));

    let exit = null;
    if (this.props.exitedY) {
      const y = g.yRowAt(this.props.exitedY-1);
      exit = <rect className="exit" x={x} y={y} width={width} height={g.yRowHeight} />
    }

    return <g className="Process">
      <rect className="body" x={x} y={y} width={width} height={height} />
      {exit}
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
}



function produceElements(delta) {
  const result = [];

  for (const pid in delta['erlang-processes']) {
    const proc = delta['erlang-processes'][pid];
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
  }
  return result;
};


export default {
  produceElements // , Component
};