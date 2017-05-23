V.SHELL_LINE_HEIGHT = 22;


class ShellIOView extends React.Component {
  render() {
    let rects = [];

    for (let i in this.props.tree.shellIO) {
      let e = this.props.tree.shellIO[i];

      // let x = proc.x*(V.CELL_WIDTH + V.CELL_GUTTER);
      // let width = V.CELL_WIDTH;
      let y = e.y*V.CELL_HEIGHT;

      let texts = [];
      for (let j in e.lines) {
        let y1 = y + V.SHELL_LINE_HEIGHT*(parseInt(j)+1) - 7;
        // if (y1 == 432) debugger
        texts.push(<text key={j} x={0} y={y1} className="shell-text">{e.lines[j]}</text>);
      }

      rects.push(<g key={i}>
        <rect x={0} y={y} width={this.props.width} height={e.height} className="shell-area" />
        <g>{texts}</g>
      </g>);
    }

    return <g>
      <g>{rects}</g>
    </g>;
  }
};

ShellIOView.propTypes = {
  tree: React.PropTypes.object.isRequired,
  width: React.PropTypes.number.isRequired,
};