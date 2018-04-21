import React from 'react';
import PropTypes from 'prop-types';

import attr from '../attr';
import Layout from '../svgLayout';



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
      exit = <rect className="process-exit" x={x} y={y} width={width} height={EXIT_HEIGHT} />
    }

    let spawnLines = null;
    let spawnOverBody = null;
    if (typeof this.props.parentX === 'number') { // might be zero
      let parentX = Math.floor(g.xColWidth/2) + g.xColStart(this.props.parentX);
      let parentY1 = y - g.yRowHeight;
      spawnLines = <line className="spawn" x1={x} y1={y+0.5} x2={parentX} y2={y+0.5} />;
      spawnOverBody = <React.Fragment>
        <line className="spawn" x1={parentX+0.5} y1={y+0.5} x2={parentX+0.5} y2={parentY1+0.5} />
        <line className="spawn" x1={parentX+0.5} y1={y+0.5} x2={parentX+PROC_WIDTH/2+0.5} y2={y+0.5} />
        <line className="spawn" x1={x} y1={y+0.5} x2={x+PROC_WIDTH} y2={y+0.5} />
      </React.Fragment>;
    }

    let bodyRect = null;
    if (this.props.selectedProcessPid === this.props.id) {
      bodyRect = <rect onClick={this.onClick} className="process-body selected" x={x+0.5} y={y+0.5} width={width-0.5*2} height={height-0.5*2} />;
    } else {
      bodyRect = <rect onClick={this.onClick} className="process-body" x={x} y={y} width={width} height={height} />;
    }

    return [
      <Layout.WithLayout key="procBody" name="procBody">
        {bodyRect}
        {exit}
      </Layout.WithLayout>,

      <Layout.WithLayout key="procSpawnLinesOverBody" name="procSpawnLinesOverBody">
        {spawnOverBody}
      </Layout.WithLayout>,

      <Layout.WithLayout key="procSpawnLines" name="procSpawnLines">
        {spawnLines}
      </Layout.WithLayout>,
    ];
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
  selectedProcessPid: PropTypes.string,
}



function produceElements(delta) {
  const result = [];

  delta['erlang-processes'].forEach((proc, pid) => {
    // skip
    if (!proc.SpawnedAt && !proc.TraceStartedAt) { return; }

    const attrs = {
      x: attr.xPid(proc.Pid),
      startedY: attr.yTimestamp(proc.SpawnedAt || proc.TraceStartedAt),
    }
    if (proc.SpawnedAt) {
      attrs.parentX = attr.xPid(proc.ParentPid);
    }

    if (proc.ExitedAt) {
      attrs.exitedY = attr.yTimestamp(proc.ExitedAt);
    } else {
      attrs.continueY = attr.yTimestampNow();
    }

    result.push({ id: pid, key: pid, Component, attrs });
  });

  return result;
};


export default {
  produceElements // , Component
};