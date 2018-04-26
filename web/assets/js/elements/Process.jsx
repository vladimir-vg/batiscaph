import React from 'react';
import PropTypes from 'prop-types';

import attr from '../attr';
import { isTracedAt } from '../delta';
import Layout from '../components/svgLayout';



// process body consists of several traced segments
// and mentions
const PROC_WIDTH = 6;
const EXIT_HEIGHT = 2;
class ProcessBody extends React.Component {
  constructor() {
    super();

    this.selectProcess = this.selectProcess.bind(this);
  }

  selectProcess() {
    if (this.props.selectedProcessPid === this.props.id) {
      this.props.selectProcess(null);
    } else {
      this.props.selectProcess(this.props.id);
    }
  }

  hoverProcess(id) { this.props.hoverProcess(id); }

  render() {
    const g = this.props.grid;
    const sideOffset = (g.xColWidth - PROC_WIDTH)/2;
    const x = g.xColStart(this.props.x) + sideOffset;
    const width = PROC_WIDTH;

    // for now only first segment is drawn
    // TODO: generalize it to several segments
    const [[startedY, continueY]] = this.props.tracedSegments;

    const y = g.yRowAt(startedY);
    const height = (g.yRowAt(continueY) - g.yRowAt(startedY));

    let exit = null;
    if (this.props.exitedY) {
      const y1 = g.yRowAt(this.props.exitedY)-EXIT_HEIGHT/2;
      exit = <rect className="process-exit" x={x} y={y1} width={width} height={EXIT_HEIGHT} />
    }

    let bodyRect = null;
    if (this.props.selectedProcessPid === this.props.id) {
      bodyRect = <rect className="process-body selected"
        onClick={this.selectProcess}
        onMouseEnter={this.hoverProcess.bind(this, this.props.id)} 
        onMouseLeave={this.hoverProcess.bind(this, null)}
        x={x+0.5} y={y+0.5} width={width-0.5*2} height={height-0.5*2} />;
    } else if (this.props.hoveredProcessPid === this.props.id) {
        bodyRect = <rect className="process-body hovered"
        onClick={this.selectProcess}
        onMouseEnter={this.hoverProcess.bind(this, this.props.id)} 
        onMouseLeave={this.hoverProcess.bind(this, null)}
        x={x+0.5} y={y+0.5} width={width-0.5*2} height={height-0.5*2} />;
    } else {
      bodyRect = <rect className="process-body"
        onClick={this.selectProcess}
        onMouseEnter={this.hoverProcess.bind(this, this.props.id)} 
        onMouseLeave={this.hoverProcess.bind(this, null)}
        x={x} y={y} width={width} height={height} />;
    }

    return [
      <Layout.WithLayout key="procBody" name="procBody">
        {bodyRect}
        {exit}
      </Layout.WithLayout>,
    ];
  }
}
ProcessBody.propTypes = {
  id: PropTypes.string.isRequired,
  grid: PropTypes.object.isRequired,
  x: PropTypes.number.isRequired,
  tracedSegments: PropTypes.array.isRequired,
  exitedY: PropTypes.number,

  selectProcess: PropTypes.func.isRequired,
  hoverProcess: PropTypes.func.isRequired,
  selectedProcessPid: PropTypes.string,
  hoveredProcessPid: PropTypes.string,
}



class SpawnLine extends React.Component {
  render() {
    const g = this.props.grid;
    const sideOffset = (g.xColWidth - PROC_WIDTH)/2;

    const y = g.yRowAt(this.props.y);
    const childX = g.xColStart(this.props.childX) + sideOffset;
    const parentX = Math.floor(g.xColWidth/2) + g.xColStart(this.props.parentX);
    const parentY1 = y - g.yRowHeight;
    const spawnOverBody = <React.Fragment>
      <line className="spawn" x1={parentX+0.5} y1={y+0.5} x2={parentX+0.5} y2={parentY1+0.5} />
      <line className="spawn" x1={parentX+0.5} y1={y+0.5} x2={parentX+PROC_WIDTH/2+0.5} y2={y+0.5} />
      <line className="spawn" x1={childX} y1={y+0.5} x2={childX+PROC_WIDTH} y2={y+0.5} />
    </React.Fragment>;

    return [
      <Layout.WithLayout key="procSpawnLinesOverBody" name="procSpawnLinesOverBody">
        {spawnOverBody}
      </Layout.WithLayout>,

      <Layout.WithLayout key="procSpawnLines" name="procSpawnLines">
        <line className="spawn" x1={childX} y1={y+0.5} x2={parentX} y2={y+0.5} />
      </Layout.WithLayout>,
    ];
  }
}
SpawnLine.propTypes = {
  grid: PropTypes.object.isRequired,
  parentX: PropTypes.number.isRequired,
  childX: PropTypes.number.isRequired,
  y: PropTypes.number.isRequired,
  isParentTraced: PropTypes.bool,
}



function produceElements(delta) {
  const result = [];

  delta['erlang-processes'].forEach((proc, pid) => {
    if (proc.SpawnedAt) {
      const key = `${proc.ParentPid} ${proc.Pid}`;
      result.push({
        id: key, key, Component: SpawnLine,
        attrs: {
          isParentTraced: isTracedAt({ delta, at: proc.SpawnedAt, pid: proc.ParentPid }),
          parentX: attr.xPid(proc.ParentPid),
          childX: attr.xPid(proc.Pid),
          y: attr.yTimestamp(proc.SpawnedAt),
        }
      });
    }

    // skip
    if (!proc.SpawnedAt && !proc.TraceStartedAt) { return; }

    const attrs = {
      x: attr.xPid(proc.Pid),
      tracedSegments: [],
    }
    if (proc.SpawnedAt || proc.TraceStartedAt) {
      // for now process may have only one traced segment
      // TODO: in future better to learn how to display multiple segments
      // when process tracing was stopped and started again
      attrs.tracedSegments.push([
        attr.yTimestamp(proc.SpawnedAt || proc.TraceStartedAt),
        (proc.ExitedAt ? attr.yTimestamp(proc.ExitedAt) : attr.yTimestampNow()),
      ]);
    }
    if (proc.ExitedAt) {
      attrs.exitedY = attr.yTimestamp(proc.ExitedAt);
    }

    result.push({ id: pid, key: pid, Component: ProcessBody, attrs });
  });

  return result;
};



export default {
  produceElements
};