


const CELL_WIDTH = 10;
const CELL_HEIGHT = 10;
const CELL_HGUTTER = 5;
const MENTION_PADDING = 2;
const SHELL_WIDTH = 460;



class ProcessElement extends React.Component {
  renderTracedPart(part, i) {
    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x;
    let y = CELL_HEIGHT*part.fromY;
    let width = CELL_WIDTH;
    let height = CELL_HEIGHT*(part.toY - part.fromY);

    let untracedNode = null;
    if (part.untracedHead) {
      let mX = x + MENTION_PADDING;
      let mWidth = width - 2*MENTION_PADDING;
      untracedNode = <g>
        <rect x={x} y={y-CELL_HEIGHT/2} width={width} height={CELL_HEIGHT/2} style={{fill: '#D9D9D9'}} />
        <rect x={mX} y={y-CELL_HEIGHT/2} width={mWidth} height={CELL_HEIGHT/2} style={{fill: '#EDEDED'}} />
      </g>;
    }

    let exitRect = null;
    if (part.exitMark) {
      let h = part.unnormalExit ? (CELL_HEIGHT/2) : 2;
      exitRect = <rect x={x} y={y+height-h} width={width} height={h} style={{fill: '#FC8888'}} />
    }
    return <g key={i}>
      {untracedNode}
      <rect x={x} y={y} width={width} height={height} style={{fill: '#D9D9D9'}} />
      {exitRect}
    </g>;
  }

  renderMentionPart(part, i) {
    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x;
    let y = CELL_HEIGHT*part.y - Math.floor(CELL_HEIGHT/2);
    let width = CELL_WIDTH;
    let height = CELL_HEIGHT;

    let mX = x + MENTION_PADDING;
    let mWidth = width - 2*MENTION_PADDING;

    return <g key={i}>
      <rect x={x} y={y} width={width} height={height} style={{fill: '#D9D9D9'}} />
      <rect x={mX} y={y} width={mWidth} height={height} style={{fill: '#EDEDED'}} />
    </g>;
  }

  renderDeadPart(part, i) {
    let crossSize = Math.min(CELL_WIDTH, CELL_HEIGHT);

    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x;
    let y = CELL_HEIGHT*part.y - CELL_HEIGHT/2;

    let x1 = x+(CELL_WIDTH-crossSize)/2;
    let y1 = y+(CELL_HEIGHT-crossSize)/2;
    let x2 = x+CELL_WIDTH-(CELL_WIDTH-crossSize)/2;
    let y2 = y+CELL_HEIGHT-(CELL_HEIGHT-crossSize)/2;

    return <g key={i}>
      <line x1={x1} y1={y1} x2={x2} y2={y2} style={{stroke: '#FC8888', strokeWidth: 2}} />
      <line x1={x2} y1={y1} x2={x1} y2={y2} style={{stroke: '#FC8888', strokeWidth: 2}} />
    </g>;
  }

  renderConnectingLine() {
    if (this.props.data.parts.length < 2) return null;
    let fromY = this.props.data.parts[0].y;
    let toY = this.props.data.parts[this.props.data.parts.length-1].y;

    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x + CELL_WIDTH/2;
    let y1 = CELL_HEIGHT*fromY - CELL_HEIGHT/2;
    let y2 = CELL_HEIGHT*toY - CELL_HEIGHT/2;

    return <line x1={x} y1={y1} x2={x} y2={y2} style={{stroke: '#EDEDED', strokeWidth: 2}} />;
  }

  render() {
    let nodes = [];
    for (const i in this.props.data.parts) {
      let part = this.props.data.parts[i];
      if (part.type == 'TRACED') {
        nodes.push(this.renderTracedPart(part, i));
      } else if (part.type == 'MENTION') {
        nodes.push(this.renderMentionPart(part, i));
      } else if (part.type == 'DEAD') {
        nodes.push(this.renderDeadPart(part, i));
      }
    }
    return <g>
      {this.renderConnectingLine()}
      {nodes}
    </g>;
  }
}



class SpawnElement extends React.Component {
  render() {
    let width = 2;
    let fromX = this.props.data.fromX, toX = this.props.data.toX;
    if (fromX < toX) {
      let x1 = (CELL_WIDTH+CELL_HGUTTER)*fromX + CELL_WIDTH;
      let y = CELL_HEIGHT*this.props.data.y;
      let x2 = (CELL_WIDTH+CELL_HGUTTER)*toX + CELL_WIDTH;
      return <g>
        <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#666666', strokeWidth: width}} />
        <line x1={x1-width/2} y1={y+width/2} x2={x1-width/2} y2={y-CELL_HEIGHT/2} style={{stroke: '#666666', strokeWidth: width}} />
        <line x1={x2-width/2} y1={y} x2={x2-width/2} y2={y+CELL_HEIGHT/2+width/2} style={{stroke: '#666666', strokeWidth: width}} />
      </g>;
    } else {
      let x1 = (CELL_WIDTH+CELL_HGUTTER)*toX;
      let y = CELL_HEIGHT*this.props.data.y;
      let x2 = (CELL_WIDTH+CELL_HGUTTER)*fromX;
      return <g>
        <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#666666', strokeWidth: width}} />
        <line x1={x1+width/2} y1={y} x2={x1+width/2} y2={y+CELL_HEIGHT/2+width/2} style={{stroke: '#666666', strokeWidth: width}} />
        <line x1={x2+width/2} y1={y+width/2} x2={x2+width/2} y2={y-CELL_HEIGHT/2} style={{stroke: '#666666', strokeWidth: width}} />
      </g>;
    }
  }
}



class LinkElement extends React.Component {
  render() {
    let xs = [this.props.data.fromX, this.props.data.toX];
    xs.sort();
    let x1 = (CELL_WIDTH+CELL_HGUTTER)*xs[0];
    let y = CELL_HEIGHT*this.props.data.y;
    let x2 = (CELL_WIDTH+CELL_HGUTTER)*xs[1] + CELL_WIDTH;
    return <g>
      <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#F2994A', strokeWidth: 2}} />
    </g>;
  }
}



class MentionElement extends React.Component {
  render() {
    let xs = [this.props.data.fromX, this.props.data.toX];
    xs.sort();
    let x1 = (CELL_WIDTH+CELL_HGUTTER)*xs[0];
    let y = CELL_HEIGHT*this.props.data.y;
    let x2 = (CELL_WIDTH+CELL_HGUTTER)*xs[1] + CELL_WIDTH;
    return <g>
      <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#EDEDED', strokeWidth: 2}} />
    </g>;
  }
}



class PointElement extends React.Component {
  render() {
    let width = CELL_WIDTH+4;
    let height = 2;

    let x = Math.floor((CELL_WIDTH+CELL_HGUTTER)*this.props.data.x + (CELL_WIDTH - width)/2);
    let y = Math.floor(CELL_HEIGHT*this.props.data.y - height/2);
    return <rect x={x} y={y} width={width} height={height} style={{fill: '#BDBDBD'}} />
  }
}



class ScenarioView extends React.Component {
  constructor() {
    super();
    this.state = {
      viewportWidth: Math.max(document.documentElement.clientWidth, window.innerWidth || 0),
      viewportHeight: Math.max(document.documentElement.clientHeight, window.innerHeight || 0),
    }
  }

  componentDidMount() {
    this.props.onInstanceIdChange(this.props.match.params.id);
  }

  componentWillReceiveProps(props) {
    if (this.props.match.params.id != props.match.params.id) {
      this.props.onInstanceIdChange(this.props.match.params.id);
    }
  }

  // gridPositionFunc(x, y) {
  //   return {x: x % (CELL_WIDTH + CELL_HGUTTER), y: y % CELL_HEIGHT};
  // }

  renderGrid() {
    let maxX = 1 + this.state.viewportWidth/(CELL_WIDTH+CELL_HGUTTER);
    let maxY = 1 + this.state.viewportHeight/CELL_HEIGHT;
    let lines = [];

    let minX = -Math.floor((SHELL_WIDTH+100)/(CELL_WIDTH+CELL_HGUTTER));
    let minY = -10;

    for (let i = minX; i < maxX; i++) {
      let xa = i*(CELL_WIDTH+CELL_HGUTTER);
      let xb = i*(CELL_WIDTH+CELL_HGUTTER) + CELL_WIDTH;
      lines.push(<line key={'ha-' + i} x1={xa+0.5} y1={minY*CELL_HEIGHT} x2={xa+0.5} y2={maxY*CELL_HEIGHT} style={{stroke: '#fee'}} />);
      lines.push(<line key={'hb-' + i} x1={xb-0.5} y1={minY*CELL_HEIGHT} x2={xb-0.5} y2={maxY*CELL_HEIGHT} style={{stroke: '#fee'}} />);
    }
    for (let i = minY; i < maxY; i++) {
      let y = i*CELL_HEIGHT;
      lines.push(<line key={'v-' + i} x1={minX*(CELL_WIDTH+CELL_HGUTTER)} y1={y+0.5} x2={maxX*(CELL_WIDTH+CELL_HGUTTER)} y2={y+0.5} style={{stroke: '#fee'}} />);
    }
    return lines;
  }

  render() {
    if (!this.props.tree) { return <div />; }

    let processes = [], spawns = [], links = [], mentions = [], points = [];
    for (let pid in this.props.tree.processes) {
      processes.push(<ProcessElement key={pid} data={this.props.tree.processes[pid]} />);
    }
    for (let key in this.props.tree.spawns) {
      spawns.push(<SpawnElement key={key} data={this.props.tree.spawns[key]} />);
    }
    for (let key in this.props.tree.links) {
      links.push(<LinkElement key={key} data={this.props.tree.links[key]} />);
    }
    for (let key in this.props.tree.points) {
      points.push(<PointElement key={key} data={this.props.tree.points[key]} />);
    }
    for (let key in this.props.tree.mentions) {
      mentions.push(<MentionElement key={key} data={this.props.tree.mentions[key]} />);
    }

    let height = this.props.tree.width*(CELL_WIDTH+CELL_HGUTTER);
    let width = this.props.tree.height*CELL_HEIGHT;

    return <div>
      <SvgView padding={100} paddingLeft={SHELL_WIDTH+100} paddedWidth={width} paddedHeight={height}>
        <g>{this.renderGrid()}</g>
        <g>{processes}</g>
        <g>{spawns}</g>
        <g>{links}</g>
        <g>{mentions}</g>
        <g>{points}</g>
      </SvgView>
      <ShellPanel width={SHELL_WIDTH} events={this.props.shellEvents} prompt={this.props.shellPrompt} submitInput={this.props.submitShellInput} />
    </div>;
  }
}


