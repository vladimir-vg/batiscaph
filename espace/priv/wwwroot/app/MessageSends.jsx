class MessageSends extends React.Component {
  onItemSelect(type, key) {
    this.props.onItemSelect({type: type, key: key});
  }

  onCellHoverEnter(type, key) {
    this.props.onItemHover({type: type, key: key});
  }
  onCellHoverLeave() {
    this.props.onItemHover(null);
  }

  renderSend(e) {
    let y = e.y*V.CELL_HEIGHT;
    let x1, x2;
    let outerX = -(V.SHELL_SIDELINE_WIDTH + V.CELL_WIDTH*2);
    if ((e.from in this.props.tree.procs) && (e.to in this.props.tree.procs)) {
      x1 = (this.props.tree.procs[e.from].x+1)*(V.CELL_WIDTH + V.CELL_GUTTER) - V.CELL_WIDTH/2;
      x2 = (this.props.tree.procs[e.to].x+1)*(V.CELL_WIDTH + V.CELL_GUTTER) - V.CELL_WIDTH/2;
    } else if (!(e.from in this.props.tree.procs)) {
      x1 = outerX;
      x2 = (this.props.tree.procs[e.to].x+1)*(V.CELL_WIDTH + V.CELL_GUTTER) - V.CELL_WIDTH/2;
    } else if (!(e.to in this.props.tree.procs)) {
      x1 = (this.props.tree.procs[e.from].x+1)*(V.CELL_WIDTH + V.CELL_GUTTER) - V.CELL_WIDTH/2;;
      x2 = outerX;
    }

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

      if (!(e.from in this.props.tree.procs) && !(e.to in this.props.tree.procs)) {
        console.error("both sender and receiver are absent", e);
        break;
      }

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
      <g>{this.renderSends()}</g>
    </g>;
  }
};

MessageSends.propTypes = {
  tree: React.PropTypes.object.isRequired,
  onItemSelect: React.PropTypes.func.isRequired,
  onItemHover: React.PropTypes.func.isRequired,
  selectedItem: React.PropTypes.object,
  hoveredItem: React.PropTypes.object,
};