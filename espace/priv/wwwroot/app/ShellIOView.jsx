V.SHELL_LINE_HEIGHT = V.CELL_HEIGHT*3;
V.SHELL_SIDELINE_WIDTH = 45;



class ShellIOView extends React.Component {
  onMouseDown(e) {
    // to disable dragging when selecting text
    e.stopPropagation();
  }

  render() {
    let blocks = [];

    for (let i in this.props.tree.shellIO) {
      let e = this.props.tree.shellIO[i];

      let y = e.y*V.CELL_HEIGHT;

      let texts = [];
      for (let j in e.lines) {
        let y1 = y + V.SHELL_LINE_HEIGHT*(parseInt(j)+1) - 7;
        texts.push(<text key={j} x={V.CELL_WIDTH/2} y={y1} className="shell-text">{e.lines[j]}</text>);
        if (e.type == 'shell_output') {
          texts.push(<text key={j + '-prompt'} x={-V.SHELL_SIDELINE_WIDTH/2} y={y1} className="shell-prompt-center unselectable">...</text>);
        } else {
          texts.push(<text key={j + '-prompt'} x={-V.SHELL_SIDELINE_WIDTH} y={y1} className="shell-prompt-left unselectable">{e.prompt}</text>);
        }
      }

      let x = -V.SHELL_SIDELINE_WIDTH;
      let width = this.props.width+V.SHELL_SIDELINE_WIDTH;

      blocks.push(<g key={i} onMouseDown={this.onMouseDown.bind(this)}>
        <rect x={x} y={y} width={width} height={e.height} className="shell-area" />
        <g>{texts}</g>
      </g>);
    }

    let x = -V.SHELL_SIDELINE_WIDTH;
    let width = V.SHELL_SIDELINE_WIDTH;
    let height = this.props.tree.maxY*V.CELL_HEIGHT;

    return <g>
      <rect x={x} y={0} width={width} height={height} className="shell-sideline" />
      <g>{blocks}</g>
    </g>;
  }
};

ShellIOView.propTypes = {
  tree: React.PropTypes.object.isRequired,
  width: React.PropTypes.number.isRequired,
};