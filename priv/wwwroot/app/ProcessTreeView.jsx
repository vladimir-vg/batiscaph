V.CELL_HEIGHT = 10;
V.CELL_WIDTH = 7;
V.CELL_GUTTER = 6;

V.PROC_BAD_REASON_HEIGHT = 3;



class ProcessTreeView extends React.Component {
  componentDidMount() {
    console.log(this.props.tree);
  }

  onRectClick(pid, e) {
    if (this.props.selectedPid == pid) {
      // toggle selection
      this.props.onProcSelect(null);
    } else {
      this.props.onProcSelect(pid);
    }
  }

  render() {
    let maxY = this.props.tree.maxY;
    let procRects = [];

    for (let pid in this.props.tree.procs) {
      let proc = this.props.tree.procs[pid];

      let x = proc.x*(V.CELL_WIDTH + V.CELL_GUTTER) + V.CELL_GUTTER;
      let y = proc.startedY*V.CELL_HEIGHT;
      let width = V.CELL_WIDTH;
      let height = ((proc.stoppedY || maxY) - proc.startedY)*V.CELL_HEIGHT;

      let className = "proc";
      if (this.props.selectedPid == pid) {
        className += " selected";
      } else if (this.props.selectedPid && this.props.tree.procs[this.props.selectedPid].ancestors.indexOf(pid) != -1) {
        className += " selected-ancestor";
      }

      let badReasonRect = null;
      if (proc.reason && proc.reason != 'normal') {
        badReasonRect = <rect x={x} y={y+height-V.PROC_BAD_REASON_HEIGHT} width={width} height={V.PROC_BAD_REASON_HEIGHT} className="bad-reason" />;
      }

      procRects.push(<g key={pid}>
        <rect x={x} y={y} width={width} height={height} onClick={this.onRectClick.bind(this, pid)} className={className} />
        {badReasonRect}
      </g>);
    }

    return <g>
      <g>{procRects}</g>
    </g>;
  }
};

ProcessTreeView.propTypes = {
  tree: React.PropTypes.object.isRequired,
  onProcSelect: React.PropTypes.func.isRequired,
  selectedPid: React.PropTypes.string,
};