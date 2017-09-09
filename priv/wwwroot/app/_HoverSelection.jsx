class HoverSelection extends React.Component {
  onItemSelect(type, key) {
    this.props.onItemSelect({type: type, key: key});
  }

  onCellHoverEnter(type, key) {
    this.props.onItemHover({type: type, key: key});
  }
  onCellHoverLeave() {
    this.props.onItemHover(null);
  }

  renderHoverSelection() {
    let rectWidth = 10000;
    let maxY = this.props.tree.maxY;
    let rects = [];

    for (let pid in this.props.tree.procs) {
      let proc = this.props.tree.procs[pid];

      let x = proc.x*(V.CELL_WIDTH + V.CELL_GUTTER) + V.CELL_GUTTER;
      let y = proc.startedY*V.CELL_HEIGHT;
      let width = V.CELL_WIDTH;
      let height = ((proc.stoppedY || maxY) - proc.startedY)*V.CELL_HEIGHT;

      rects.push(<rect key={'start-'+pid}
        x={0} y={y-V.CELL_HEIGHT/2} width={rectWidth} height={V.CELL_HEIGHT}
        onMouseEnter={this.onCellHoverEnter.bind(this, 'proc', pid)} onMouseLeave={this.onCellHoverLeave.bind(this)}
        onClick={this.onItemSelect.bind(this, 'proc', pid)} className="cell-hover" />);
      rects.push(<rect key={'end-'+pid}
        x={0} y={y+height-V.CELL_HEIGHT/2} width={rectWidth} height={V.CELL_HEIGHT}
        onMouseEnter={this.onCellHoverEnter.bind(this, 'proc', pid)} onMouseLeave={this.onCellHoverLeave.bind(this)}
        onClick={this.onItemSelect.bind(this, 'proc', pid)} className="cell-hover" />);
    }

    for (let i in this.props.tree.sends) {
      let e = this.props.tree.sends[i];
      let y = e.y*V.CELL_HEIGHT;

      let className = "cell-hover";
      if (this.props.selectedItem && this.props.selectedItem.type == 'send' && this.props.selectedItem.key == i) {
        className += " selected";
      }

      rects.push(<rect key={'send-'+e.y+'-'+e.from+'-'+e.to}
        x={-V.CELL_WIDTH} y={y-V.CELL_HEIGHT/2} width={rectWidth} height={V.CELL_HEIGHT}
        onMouseEnter={this.onCellHoverEnter.bind(this, 'send', i)} onMouseLeave={this.onCellHoverLeave.bind(this)}
        onClick={this.onItemSelect.bind(this, 'send', i)} className={className} />);
    }

    return rects;
  }

  render() {
    return <g>
      <g>{this.renderHoverSelection()}</g>
    </g>;
  }
};

HoverSelection.propTypes = {
  tree: React.PropTypes.object.isRequired,
  onItemSelect: React.PropTypes.func.isRequired,
  onItemHover: React.PropTypes.func.isRequired,
  selectedItem: React.PropTypes.object,
  hoveredItem: React.PropTypes.object,
};