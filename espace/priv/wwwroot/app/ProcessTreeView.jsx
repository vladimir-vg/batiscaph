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

  renderProcs() {
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

      let spawnLine = null;
      if (proc.parent in this.props.tree.procs) {
        let parentProc = this.props.tree.procs[proc.parent];
        let x1 = (parentProc.x+1)*(V.CELL_WIDTH + V.CELL_GUTTER);
        let x2 = x + V.CELL_WIDTH;
        let dashArray = ""+V.CELL_GUTTER+"," + V.CELL_WIDTH;
        spawnLine = <g>
          <line x1={x1-0.5} y1={y+0.5} x2={x1-0.5} y2={y-(V.CELL_WIDTH/2)+0.5} className="spawn-line" />
          <line x1={x1} y1={y+0.5} x2={x2} y2={y+0.5} className="spawn-line" style={{strokeDasharray: dashArray}} />
          <line x1={x} y1={y+0.5} x2={x2} y2={y+0.5} className="spawn-line" />
          <line x1={x2-0.5} y1={y+0.5} x2={x2-0.5} y2={y+(V.CELL_WIDTH/2)+0.5} className="spawn-line" />
        </g>;
      }

      let badReasonRect = null;
      if (proc.reason && proc.reason != 'normal') {
        badReasonRect = <rect x={x} y={y+height-V.PROC_BAD_REASON_HEIGHT} width={width} height={V.PROC_BAD_REASON_HEIGHT} className="bad-reason" />;
      }

      procRects.push(<g key={pid}>
        <g onClick={this.onRectClick.bind(this, pid)}>
          <rect x={x} y={y} width={width} height={height} className={className} />
          {badReasonRect}
        </g>
        {spawnLine}
      </g>);
    }

    return procRects;
  }

  renderSend(e) {
    let y = e.y*V.CELL_HEIGHT;
    let x1 = (this.props.tree.procs[e.from].x+1)*(V.CELL_WIDTH + V.CELL_GUTTER) - V.CELL_WIDTH/2;
    let x2 = (this.props.tree.procs[e.to].x+1)*(V.CELL_WIDTH + V.CELL_GUTTER) - V.CELL_WIDTH/2;

    return <g key={y}>
      <line x1={x1} y1={y-0.5} x2={x2} y2={y-0.5} className="message-send" />
    </g>;
  }

  renderSelfSend(e) {
    let y = e.y*V.CELL_HEIGHT;
    let x = (this.props.tree.procs[e.from].x+1)*(V.CELL_WIDTH + V.CELL_GUTTER);
    let d = Math.min(V.CELL_HEIGHT, V.CELL_WIDTH + V.CELL_GUTTER)*0.7;
    let r = d/2;

    let dpath = "M"+(x-r)+" "+(y)+" A "+r+" "+r+", 0, 1, 1, "+(x)+" "+(y+r)+" L "+(x-3)+" "+(y+r);
    return <g key={y}>
      <path d={dpath} className="message-self-send" />
    </g>;
  }

  renderSends() {
    let sends = [];

    for (let i in this.props.tree.sends) {
      let e = this.props.tree.sends[i];
      if (e.from == e.to) {
        sends.push(this.renderSelfSend(e))
      } else {
        sends.push(this.renderSend(e));
      }
    }

    return sends;
  }

  render() {
    return <g>
      <g>{this.renderProcs()}</g>
      <g>{this.renderSends()}</g>
    </g>;
  }
};

ProcessTreeView.propTypes = {
  tree: React.PropTypes.object.isRequired,
  onProcSelect: React.PropTypes.func.isRequired,
  selectedPid: React.PropTypes.string,
};