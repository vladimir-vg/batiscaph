import React from 'react';
import PropTypes from 'prop-types';



export default class SvgView extends React.Component {
  constructor(props) {
    super();
    this.state = {
      areaCursor: null,

      posX: props.paddingLeft || props.padding,
      posY: props.padding
    }
  }

  onMouseDown(e) {
    this.startDragging(e);
  }

  onMouseMove(e) {
    this._lastMousePosX = e.clientX;
    this._lastMousePosY = e.clientY;
  
    this.moveDragging(e);
  }

  onMouseUp() {
    this.stopDragging();
  }

  onMouseLeave() {
    this.stopDragging();
  }

  onWheel(e) {
    const x = this.state.posX;
    const y = this.state.posY - e.deltaY;
    const t = this.sanitizeXY(x, y);
    this.setState({posY: t.y});
  }

  startDragging(e) {
    this._isDragging = true;
    this._dragStartX = e.clientX;
    this._dragStartY = e.clientY;
    this.setState({areaCursor: 'move'});
  }

  moveDragging(e) {
    if (!this._isDragging) return;
    if (this._posMoveAnimationRequest) return;
  
    // calculating difference from dragging start position, add to initial
    const x = this.state.posX + (e.clientX - this._dragStartX);
    const y = this.state.posY + (e.clientY - this._dragStartY);
  
    this.setPosition(x,y);
  }

  // do not allow to move out of padded space
  sanitizeXY(x,y) {
    const viewportWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
    const viewportHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);

    const paddingLeft = this.props.paddingLeft || this.props.padding;

    if ((viewportWidth+(-x)) > (this.props.paddedWidth + this.props.padding)) {
      x = -(this.props.paddedWidth-viewportWidth+this.props.padding);
    }
    if (x > paddingLeft) { x = paddingLeft; }

    if ((viewportHeight+(-y)) > (this.props.paddedHeight + this.props.padding)) {
      y = -(this.props.paddedHeight-viewportHeight+this.props.padding);
    }
    if (y > this.props.padding) { y = this.props.padding; }

    return {x: x, y: y};
  }

  setPosition(x0, y0) {
    const t = this.sanitizeXY(x0, y0);
    const { x, y } = t;

    if (x == this._dragX && y == this._dragY) return;

    this._posMoveAnimationRequest = window.requestAnimationFrame((function () {
      // explicitly setting svg figure offset
      this.posBase.transform.baseVal.getItem(0).setTranslate(x,y);
      this._dragX = x;
      this._dragY = y;

      this._posMoveAnimationRequest = undefined;
    }).bind(this))
  }

  stopDragging() {
    this._isDragging = false;
    if (this._dragX && this._dragY) {
      this.setState({posX: this._dragX, posY: this._dragY});
      this._dragX = null;
      this._dragY = null;
    }
    this.setState({areaCursor: null});
  }

  render() {
    // Commented out code that was handling dragging and scrolling inside <svg>
    return <svg className={this.props.className}
        style={{width: '100%', height: '100%'}}
        onWheel={this.onWheel.bind(this)}
        onMouseMove={this.onMouseMove.bind(this)} onMouseDown={this.onMouseDown.bind(this)}
        onMouseUp={this.onMouseUp.bind(this)} onMouseLeave={this.onMouseLeave.bind(this)}>

      <defs>
        <marker id="marker-send-start" markerWidth="8" markerHeight="8" refX="5" refY="5">
          <circle cx="5" cy="5" r="2" className="marker-send-start" />
        </marker>

        <marker id="marker-send-end" markerWidth="11" markerHeight="11" refX="6" refY="4.5" orient="auto">
          <path d="M0,2 L0,7 L6,4 L0,2" className="marker-send-end" />
        </marker>

        <marker id="marker-send-start-active" markerWidth="8" markerHeight="8" refX="5" refY="5">
          <circle cx="5" cy="5" r="2" className="marker-send-start active" />
        </marker>

        <marker id="marker-send-end-active" markerWidth="11" markerHeight="11" refX="6" refY="4.5" orient="auto">
          <path d="M0,2 L0,7 L6,4 L0,2" className="marker-send-end active" />
        </marker>
      </defs>

      {/* transform={"translate("+this.props.padding+","+this.props.padding+")"} */}
      <g ref={(ref) => { this.posBase = ref; }} transform={"translate("+this.state.posX+","+this.state.posY+")"}>
        {this.props.children}
      </g>
    </svg>;
  }
}
SvgView.propTypes = {
  padding: PropTypes.number.isRequired,
  paddedWidth: PropTypes.number.isRequired,
  paddedHeight: PropTypes.number.isRequired,
  className: PropTypes.any,
};
   