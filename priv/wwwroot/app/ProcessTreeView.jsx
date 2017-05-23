V.CELL_HEIGHT = 4;
V.CELL_WIDTH = 6;
V.CELL_GUTTER = 6;



class ProcessTreeView extends React.Component {
  componentDidMount() {
    console.log(this.props.tree);
  }

  render() {
    let maxY = this.props.tree.maxY;
    let procRects = [];

    for (let pid in this.props.tree.procs) {
      let proc = this.props.tree.procs[pid];

      let x = proc.x*(V.CELL_WIDTH + V.CELL_GUTTER);
      let y = proc.startedY*V.CELL_HEIGHT;
      let width = V.CELL_WIDTH;
      let height = ((proc.stoppedY || maxY) - proc.startedY)*V.CELL_HEIGHT;

      procRects.push(<rect key={pid} x={x} y={y} width={width} height={height} className="proc" />);
    }

    return <g>
      <g>{procRects}</g>
    </g>;
  }
};

ProcessTreeView.propTypes = {
  tree: React.PropTypes.object.isRequired
};