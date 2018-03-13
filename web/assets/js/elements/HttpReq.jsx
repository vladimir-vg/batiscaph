// this element provides rules for generating layout
import React from 'react';
import PropTypes from 'prop-types';



// this function extracts all request elements from delta
function produceElements(delta) {
  const result = [];

  if (delta['plug:requests']) {
    for (const id in delta['plug:requests']) {
      const req = delta['plug:requests'][id];
      result.push({
        id: id,
        x1: req.Pid,
        y1: req.StartedAt,
        y2: req.StoppedAt,
      });
    }
  }

  return result;
};



class Component extends React.Component {
  render() {
    const g = this.props.grid;
    const HPADDING_OFFSET = 0;
    const VPADDING_OFFSET = 0;
    const x = g.xColStart(this.props.x1)-HPADDING_OFFSET;
    const width = g.xColWidth+2*HPADDING_OFFSET;
    const y = g.yRowAt(this.props.y1)-VPADDING_OFFSET;
    const height = (g.yRowAt(this.props.y2) - g.yRowAt(this.props.y1))+2*VPADDING_OFFSET;

    // currently not possble to draw border inside rect
    // have to correct offset by hand
    const borderWidth = 1;
    return <rect className="HttpReq" style={{strokeWidth: borderWidth}}
      x={x-borderWidth/2} y={y-borderWidth/2} width={width+borderWidth} height={height+borderWidth} />;
  }
}
Component.propTypes = {
  id: PropTypes.string.isRequired,
  x1: PropTypes.number.isRequired,
  y1: PropTypes.number.isRequired,
  y2: PropTypes.number.isRequired,

  grid: PropTypes.object.isRequired,
}



export default {
  produceElements, Component
};