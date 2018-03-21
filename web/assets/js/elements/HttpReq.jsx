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
  constructor() {
    super();

    this.onRequestSelect = this.onRequestSelect.bind(this);
    this.onRequestHover = this.onRequestHover.bind(this);
  }

  onRequestSelect(id) { this.props.onRequestSelect(id); }
  onRequestHover(id) { this.props.onRequestHover(id); }

  render() {
    const g = this.props.grid;
    const HPADDING_OFFSET = 0;
    const VPADDING_OFFSET = 0;
    const x = g.xColStart(this.props.x1)-HPADDING_OFFSET;
    const width = g.xColWidth+2*HPADDING_OFFSET;
    const y = g.yRowAt(this.props.y1)-VPADDING_OFFSET;
    const height = (g.yRowAt(this.props.y2) - g.yRowAt(this.props.y1))+2*VPADDING_OFFSET;

    let className = "HttpReq";
    if (this.props.id === this.props.selectedRequestId) {
      className += " current";
    } else if (this.props.id === this.props.hoveredRequestId) {
      className += " hovered";
    }

    // currently not possble to draw border inside rect
    // have to correct offset by hand
    const borderWidth = 1;
    return <rect className={className} style={{strokeWidth: borderWidth}}
      onMouseEnter={this.onRequestHover.bind(this, this.props.id)}
      onMouseLeave={this.onRequestHover.bind(this, null)}
      onClick={this.onRequestSelect.bind(this, this.props.id)}
      x={x-borderWidth/2} y={y-borderWidth/2} width={width+borderWidth} height={height+borderWidth} />;
  }
}
Component.propTypes = {
  id: PropTypes.string.isRequired,
  x1: PropTypes.number.isRequired,
  y1: PropTypes.number.isRequired,
  y2: PropTypes.number.isRequired,

  grid: PropTypes.object.isRequired,
  selectedRequestId: PropTypes.string,
  hoveredRequestId: PropTypes.string,
  onRequestSelect: PropTypes.func.isRequired,
  onRequestHover: PropTypes.func.isRequired,
}



export default {
  produceElements, Component
};