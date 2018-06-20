// this element provides rules for generating layout
import React from 'react';
import PropTypes from 'prop-types';

import attr from '../attr';
import Layout from '../components/svgLayout';



class Component extends React.Component {
  constructor() {
    super();

    this.selectCallback = this.selectCallback.bind(this);
    this.hoverCallback = this.hoverCallback.bind(this);
  }

  selectCallback() {
    if (this.props.selectedCallbackId === this.props.id) {
      this.props.selectCallback(null);
    } else {
      this.props.selectCallback(this.props.id);
    }
  }

  hoverCallback(id) { this.props.hoverCallback(id); }

  render() {
    const g = this.props.grid;
    const HPADDING_OFFSET = 0;
    const VPADDING_OFFSET = 0;
    const x = g.xColStart(this.props.x)-HPADDING_OFFSET;
    const width = g.xColWidth+2*HPADDING_OFFSET;
    const y = g.yRowAt(this.props.y1)-VPADDING_OFFSET;
    const height = (g.yRowAt(this.props.y2) - g.yRowAt(this.props.y1))+2*VPADDING_OFFSET;

    let className = "callback";
    let highlighted = false;
    if (this.props.id === this.props.selectedCallbackId) {
      className += " current";
      highlighted = true;
    } else if (this.props.id === this.props.hoveredCallbackId) {
      className += " hovered";
      highlighted = true;
    }

    // currently not possble to draw border inside rect
    // have to correct offset by hand
    const borderWidth = 1;

    const content = <rect className={className} style={{strokeWidth: borderWidth}}
      onMouseEnter={this.hoverCallback.bind(this, this.props.id)}
      onMouseLeave={this.hoverCallback.bind(this, null)}
      onClick={this.selectCallback}
      x={x-borderWidth/2} y={y-borderWidth/2} width={width+borderWidth} height={height+borderWidth} />;

    return [
      <Layout.WithLayout key="callbackRects" name="callbackRects">
        {!highlighted && content}
      </Layout.WithLayout>,
      <Layout.WithLayout key="callbackActiveRects" name="callbackActiveRects">
        {highlighted && content}
      </Layout.WithLayout>,
    ];
  }
}
Component.propTypes = {
  id: PropTypes.string.isRequired,
  x: PropTypes.number.isRequired,
  y1: PropTypes.number.isRequired,
  y2: PropTypes.number.isRequired,

  grid: PropTypes.object.isRequired,
  selectedCallbackId: PropTypes.string,
  hoveredCallbackId: PropTypes.string,
  selectCallback: PropTypes.func.isRequired,
  hoverCallback: PropTypes.func.isRequired,
}



class SelectionBackgroundComponent extends React.Component {
  render() {
    const g = this.props.grid;

    const x = g.xColStart(this.props.x);
    const y1 = g.yRowAt(this.props.y1);
    const y2 = g.yRowAt(this.props.y2);
    const minX = -10000;
    const maxX = 10000;
    const minY = -10000;
    const maxY = 10000;

    return <Layout.WithLayout key="selectedItemBackground" name="selectedItemBackground">
      <rect x={x} y={minY} width={g.xColWidth} height={maxY-minY} className="background-selection" />
      <rect x={minX} y={y1} width={maxX-minX} height={y2-y1} className="background-selection" />
    </Layout.WithLayout>;
  }
}
SelectionBackgroundComponent.propTypes = {
  x: PropTypes.number.isRequired,
  y1: PropTypes.number.isRequired,
  y2: PropTypes.number.isRequired,

  grid: PropTypes.object.isRequired,
}



// this function extracts all request elements from delta
function produceElements(delta) {
  const result = {};

  delta['plug_requests'].forEach((req, id) => {
    const id1 = `plug ${id}`;
    result[id] = {
      id: id1,
      key: id,
      Component,
      SelectionBackgroundComponent,
      attrs: {
        x: attr.xPid(req.Pid),
        y1: attr.yTimestamp(req.StartedAt),
        y2: attr.yTimestamp(req.StoppedAt),
      }
    };
  });

  delta['cowboy_requests'].forEach((req, id) => {
    const id1 = `cowboy ${id}`;
    result[id1] = {
      id: id1,
      key: `${id} init`,
      Component,
      SelectionBackgroundComponent,
      attrs: {
        x: attr.xPid(req.Pid),
        y1: attr.yTimestamp(req.init.StartedAt),
        y2: attr.yTimestamp(req.init.StoppedAt),
      }
    };
    // if (req.handle) {
    //   result[id1] = {
    //     id: id1,
    //     key: `${id} handle`,
    //     Component,
    //     SelectionBackgroundComponent,
    //     attrs: {
    //       x: attr.xPid(req.Pid),
    //       y1: attr.yTimestamp(req.handle.StartedAt),
    //       y2: attr.yTimestamp(req.handle.StoppedAt),
    //     }
    //   };
    // }
  });

  return result;
};



export default {
  produceElements
};
