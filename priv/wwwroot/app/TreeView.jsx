const CELL_HEIGHT = 4;
const CELL_WIDTH = 6;
const CELL_GUTTER = 6;



class TreeView extends React.Component {
  componentDidMount() {
    console.log(this.props.tree);
  }

  render() {
    let maxY = this.props.tree.maxY;
    let procRects = [];

    for (let pid in this.props.tree.procs) {
      let proc = this.props.tree.procs[pid];

      let x = proc.x*(CELL_WIDTH + CELL_GUTTER);
      let y = proc.startedY*CELL_HEIGHT;
      let width = CELL_WIDTH;
      let height = ((proc.stoppedY || maxY) - proc.startedY)*CELL_HEIGHT;

      procRects.push(<rect key={pid} x={x} y={y} width={width} height={height} style={{fill: '#ccc'}} />);
    }

    return <g>
      <g>{procRects}</g>
    </g>;
  }
};

TreeView.propTypes = {
  tree: React.PropTypes.object.isRequired
};