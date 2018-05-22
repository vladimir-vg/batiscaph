// this element provides rules for generating layout
import React from 'react';
import PropTypes from 'prop-types';

import attr from '../attr';
import Layout from '../components/svgLayout';



class Component extends React.Component {
  constructor() {
    super();

    this.hoverLogEvent = this.hoverLogEvent.bind(this);
  }

  hoverLogEvent(id) { this.props.hoverLogEvent(id); }

  render() {
    const g = this.props.grid;

    const x = g.xColStart(this.props.x);
    const width = g.xColWidth;
    const y = g.yRowAt(this.props.y);
    const height = 2;

    let className = 'log-body';
    if (this.props.hoveredLogId === this.props.id) {
      className += ' hovered';
    }

    return <Layout.WithLayout key="logEvents" name="logEvents">
      <rect x={x+1} y={y} width={width-2} height={height} className={className}
        onMouseEnter={this.hoverLogEvent.bind(this, this.props.id)}
        onMouseLeave={this.hoverLogEvent.bind(this, null)} />
    </Layout.WithLayout>;
  }
}
Component.propTypes = {
  id: PropTypes.string.isRequired,
  x: PropTypes.number.isRequired,
  y: PropTypes.number.isRequired,

  grid: PropTypes.object.isRequired,
  hoveredLogId: PropTypes.string,
  hoverLogEvent: PropTypes.func.isRequired,
}



class SelectionBackgroundComponent extends React.Component {
  render() {
    const g = this.props.grid;

    const x = g.xColStart(this.props.x);
    const y = g.yRowAt(this.props.y);
    const minX = -10000;
    const maxX = 10000;
    const minY = -10000;
    const maxY = 10000;

    return <Layout.WithLayout key="selectedItemBackground" name="selectedItemBackground">
      <line x1={minX} y1={y+1} x2={maxX} y2={y+1} className="background-selection" style={{strokeWidth: 2}} />
      <rect x={x+1} y={minY} width={g.xColWidth-2} height={maxY-minY} className="background-selection" />
    </Layout.WithLayout>;
  }
}
SelectionBackgroundComponent.propTypes = {
  x: PropTypes.number.isRequired,
  y: PropTypes.number.isRequired,

  grid: PropTypes.object.isRequired,
}



// this function extracts all request elements from delta
function produceElements(delta) {
  const result = {};

  delta['lager-events'].forEach((e, id) => {
    result[id] = {
      id: id,
      key: id,
      Component,
      SelectionBackgroundComponent,
      attrs: {
        x: attr.xPid(e.Pid),
        y: attr.yTimestamp(e.At),
      }
    };
  });

  return result;
};



export default {
  produceElements
};
