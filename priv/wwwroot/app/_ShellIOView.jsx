V.SHELL_LINE_HEIGHT = V.CELL_HEIGHT*3;
V.SHELL_LINE_TOP_PADDING = V.CELL_HEIGHT;
V.SHELL_SIDELINE_WIDTH = 45;



class ShellIOView extends React.Component {
  // onMouseDown(e) {
  //   // to disable dragging when selecting text
  //   e.stopPropagation();
  // }

  onItemSelect(type, key) {
    this.props.onItemSelect({type: type, key: key});
  }

  onCellHoverEnter(type, key) {
    this.props.onItemHover({type: type, key: key});
  }
  onCellHoverLeave() {
    this.props.onItemHover(null);
  }

  renderTextContent(text) {
    let parts = [];
    let onText = (text, pos) => {
      parts.push(<tspan key={pos}>{text}</tspan>);
    }

    let onPid = (pid, pos) => {
      let className = "pid";
      if (this.props.hoveredItem && this.props.hoveredItem.type == 'proc' && this.props.hoveredItem.key == pid) {
        className += " hovered";
      }

      if (this.props.selectedItem && this.props.selectedItem.type == 'proc' && this.props.selectedItem.key == pid) {
        className += " selected";
      }

      parts.push(<tspan key={pos} className={className}
        onClick={this.onItemSelect.bind(this, 'proc', pid)}
        onMouseEnter={this.onCellHoverEnter.bind(this, 'proc', pid)}
        onMouseLeave={this.onCellHoverLeave.bind(this)}>{pid}</tspan>);
    }

    V.walkInfoText(text, onText, onPid);

    return parts;
  }

  render() {
    let blocks = [];

    for (let i in this.props.tree.shellIO) {
      let e = this.props.tree.shellIO[i];

      let y = e.y*V.CELL_HEIGHT;

      let texts = [];
      let n = 1;
      for (let j in e.blocks) {
        for (let k in e.blocks[j].lines) {
          let line = e.blocks[j].lines[k];
          let prompt = e.blocks[j].prompt;
          let y1 = y + V.SHELL_LINE_HEIGHT*n - V.SHELL_LINE_TOP_PADDING;

          let className = "shell-text";
          if (prompt) {
            texts.push(<text key={'prompt-' + n} x={-V.SHELL_SIDELINE_WIDTH} y={y1} className="shell-prompt-left unselectable">{prompt}</text>);
          } else {
            texts.push(<text key={'prompt-' + n} x={-V.SHELL_SIDELINE_WIDTH/2} y={y1} className="shell-prompt-center unselectable muted">...</text>);
            className += " muted";
          }
          texts.push(<text key={n} x={V.CELL_WIDTH} y={y1} className={className}>{this.renderTextContent(line)}</text>);

          n += 1;
        }
      }

      let x = -V.SHELL_SIDELINE_WIDTH;
      // let width = this.props.width+V.SHELL_SIDELINE_WIDTH;
      let height = e.height;

      let areaWidth = 10000; // right to the end of visible space

      // onMouseDown={this.onMouseDown.bind(this)}
      blocks.push(<g key={i}>
        <rect x={x} y={y} width={areaWidth} height={height} className="shell-area" />
        <line x1={x} y1={y+0.5} x2={x+areaWidth} y2={y+0.5} className="shell-area-border" />
        <line x1={x} y1={height+y-0.5} x2={x+areaWidth} y2={height+y-0.5} className="shell-area-border" />
        <g>{texts}</g>
      </g>);
    }

    let x = -V.SHELL_SIDELINE_WIDTH;
    let width = V.SHELL_SIDELINE_WIDTH;
    let height = this.props.tree.maxY*V.CELL_HEIGHT;

    return <g>
      <rect x={x} y={-V.CELL_HEIGHT/2} width={width} height={height+V.CELL_HEIGHT} className="shell-sideline" />
      <g>{blocks}</g>
    </g>;
  }
};

ShellIOView.propTypes = {
  tree: React.PropTypes.object.isRequired,
  width: React.PropTypes.number.isRequired,
  onItemSelect: React.PropTypes.func.isRequired,
  onItemHover: React.PropTypes.func.isRequired,
  selectedItem: React.PropTypes.object,
  hoveredItem: React.PropTypes.object,
};