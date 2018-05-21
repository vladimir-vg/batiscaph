// this element provides rules for generating layout
import React from 'react';
import PropTypes from 'prop-types';

import attr from '../attr';
import Layout from '../components/svgLayout';



class Component extends React.Component {
  render() {
    const g = this.props.grid;

    const x = g.xColStart(this.props.x);
    const width = g.xColWidth;
    const y = g.yRowAt(this.props.y);
    const height = 2;

    return <Layout.WithLayout key="logEvents" name="logEvents">
      <rect x={x+1} y={y} width={width-2} height={height} className="log-body" />
    </Layout.WithLayout>;
  }
}
Component.propTypes = {
  id: PropTypes.string.isRequired,
  x: PropTypes.number.isRequired,
  y: PropTypes.number.isRequired,

  grid: PropTypes.object.isRequired,
}



// this function extracts all request elements from delta
function produceElements(delta) {
  const result = [];

  delta['lager-events'].forEach((e, id) => {
    result.push({
      id: id,
      key: id,
      Component,
      attrs: {
        x: attr.xPid(e.Pid),
        y: attr.yTimestamp(e.At),
      }
    });
  });

  return result;
};



export default {
  produceElements
};
