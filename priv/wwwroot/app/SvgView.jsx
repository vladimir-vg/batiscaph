class SvgView extends React.Component {
  constructor() {
    super();
    this.state = {
      areaCursor: null,

      // global position of the whole figure
      posX: 0,
      posY: 0
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
    var x = this.state.posX + (e.clientX - this._dragStartX);
    var y = this.state.posY + (e.clientY - this._dragStartY);

    this._posMoveAnimationRequest = window.requestAnimationFrame((function () {
      // explicitly setting svg figure offset
      this.refs.posBase.transform.baseVal.getItem(0).setTranslate(x,y);
      this.refs.vposBase.transform.baseVal.getItem(0).setTranslate(0,y);
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
    var verticallyMovingChildren = [];
    var allMovingChildren = [];

    React.Children.forEach(this.props.children, function (child) {
      if (child.props.moveOnlyVertically) {
        verticallyMovingChildren.push(child);
      } else {
        allMovingChildren.push(child);
      }
    });

    return <svg ref="svg"
      style={{position: 'fixed', top: 0, left: 0, width:'100%', height: '100%', cursor: this.state.areaCursor}}
      onMouseMove={this.onMouseMove.bind(this)} onMouseDown={this.onMouseDown.bind(this)}
      onMouseUp={this.onMouseUp.bind(this)} onMouseLeave={this.onMouseLeave.bind(this)}>
  
      <g ref="vposBase" transform={"translate(0,"+this.state.posY+")"}>
        {verticallyMovingChildren}
      </g>

      <g ref="posBase" transform={"translate("+this.state.posX+","+this.state.posY+")"}>
        {allMovingChildren}
      </g>
    </svg>;
  }
}
