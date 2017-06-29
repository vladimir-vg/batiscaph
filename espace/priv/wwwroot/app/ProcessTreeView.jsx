V.CELL_HEIGHT = 10;
V.CELL_WIDTH = 7;
V.CELL_GUTTER = 6;

V.PROC_BAD_REASON_HEIGHT = 3;



class ProcessTreeView extends React.Component {
  componentDidMount() {
    console.log(this.props.tree);
  }

  onItemSelect(type, key) {
    this.props.onItemSelect({type: type, key: key});
  }

  onCellHoverEnter(type, key) {
    this.props.onItemHover({type: type, key: key});
  }
  onCellHoverLeave() {
    this.props.onItemHover(null);
  }

  renderSpawnLine(x, y, proc) {
    let spawnLine = null;
    if (proc.parent in this.props.tree.procs) {
      let parentProc = this.props.tree.procs[proc.parent];
      let x1 = (parentProc.x+1)*(V.CELL_WIDTH + V.CELL_GUTTER);
      let x2 = x + V.CELL_WIDTH;
      let dashArray = ""+V.CELL_GUTTER+"," + V.CELL_WIDTH;
      return <g>
        <line x1={x1-0.5} y1={y+0.5} x2={x1-0.5} y2={y-(V.CELL_WIDTH/2)+0.5} className="spawn-line" />
        <line x1={x1} y1={y+0.5} x2={x2} y2={y+0.5} className="spawn-line" style={{strokeDasharray: dashArray}} />
        <line x1={x} y1={y+0.5} x2={x2} y2={y+0.5} className="spawn-line" />
        <line x1={x2-0.5} y1={y+0.5} x2={x2-0.5} y2={y+(V.CELL_WIDTH/2)+0.5} className="spawn-line" />
      </g>;
    }

    return null;
  }

  renderProcs() {
    let maxY = this.props.tree.maxY;
    let procRects = [];

    for (let pid in this.props.tree.procs) {
      let proc = this.props.tree.procs[pid];

      let x = proc.x*(V.CELL_WIDTH + V.CELL_GUTTER) + V.CELL_GUTTER;
      let y = proc.startedY*V.CELL_HEIGHT;
      let width = V.CELL_WIDTH;
      let height = ((proc.stoppedY || maxY) - proc.startedY)*V.CELL_HEIGHT;

      let className = "";
      if (this.props.hoveredItem && this.props.hoveredItem.type == 'proc' && this.props.hoveredItem.key == pid) {
        className += " hovered";
      }

      if (this.props.selectedItem && this.props.selectedItem.type == 'proc' && this.props.selectedItem.key == pid) {
        className += " selected";
      }

      let spawnLine = this.renderSpawnLine(x, y, proc);

      let badReasonRect = null;
      if (proc.reason && proc.reason != 'normal') {
        badReasonRect = <rect x={x} y={y+height-V.PROC_BAD_REASON_HEIGHT} width={width} height={V.PROC_BAD_REASON_HEIGHT} className="bad-reason" />;
      }

      procRects.push(<g key={pid} className={className}>
        <g onClick={this.onItemSelect.bind(this, 'proc', pid)}
            onMouseEnter={this.onCellHoverEnter.bind(this, 'proc', proc.pid)}
            onMouseLeave={this.onCellHoverLeave.bind(this)}>
          <rect x={x} y={y} width={width} height={height} className="proc" />
          {badReasonRect}
        </g>
        {spawnLine}
      </g>);
    }

    return procRects;
  }

  render() {
    return <g>
      <g>{this.renderProcs()}</g>
    </g>;
  }
};

ProcessTreeView.propTypes = {
  tree: React.PropTypes.object.isRequired,
  onItemSelect: React.PropTypes.func.isRequired,
  onItemHover: React.PropTypes.func.isRequired,
  selectedItem: React.PropTypes.object,
  hoveredItem: React.PropTypes.object,
};