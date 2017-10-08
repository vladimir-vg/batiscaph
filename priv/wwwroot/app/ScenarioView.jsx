


const CELL_WIDTH = 10;
const CELL_HEIGHT = 10;
const CELL_HGUTTER = 5;
const MENTION_PADDING = 2;
const SHELL_WIDTH = 460;



class Layer extends React.Component {
  constructor() {
    super();
    this.state = {
      node: null
    }
  }

  componentDidMount() {
    this.setState({node: this.context.getLayerNode(this.props.name)});
  }

  render() {
    if (!this.state.node) {
      return null;
    }

    // let node = this.context.getLayerNode(this.props.name);
    return ReactDOM.createPortal(this.props.children, this.state.node);
  }
}
Layer.contextTypes = {
  getLayerNode: PropTypes.func
}
Layer.propTypes = {
  name: PropTypes.string
};



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
    let y = CELL_HEIGHT*part.y;
    let y1 = y - Math.floor(CELL_HEIGHT/2);
    let width = CELL_WIDTH;
    let height = CELL_HEIGHT;

    let mX = x + MENTION_PADDING;
    let mWidth = width - 2*MENTION_PADDING;

    // render only first and last mention in process
    // all others supposed to be marked with small grey dots
    if (i == 0 || i == this.props.data.parts.length-1) {
      return <g key={i}>
        <rect x={x} y={y1} width={width} height={height} style={{fill: '#D9D9D9'}} />
        <rect x={mX} y={y1} width={mWidth} height={height} style={{fill: '#EDEDED'}} />
      </g>
    }

    return <circle key={i} cx={x+CELL_WIDTH/2} cy={y} r={2} style={{fill: '#D9D9D9'}} />;
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
    let startY = this.props.data.startY;
    let stopY = this.props.data.stopY;

    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x + CELL_WIDTH/2;
    let y1 = CELL_HEIGHT*startY - CELL_HEIGHT/2;
    let y2 = CELL_HEIGHT*stopY - CELL_HEIGHT/2;

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
    return <Layer name="processes">
      {this.renderConnectingLine()}
      {nodes}
    </Layer>;
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
      return [
        <Layer key="beforeProcesses" name="beforeProcesses">
          <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#666666', strokeWidth: width}} />
        </Layer>,
        <Layer key="afterProcesses" name="afterProcesses">
          <line x1={x1-width/2} y1={y+width/2} x2={x1-width/2} y2={y-CELL_HEIGHT/2} style={{stroke: '#666666', strokeWidth: width}} />
          <line x1={x2-width/2} y1={y} x2={x2-width/2} y2={y+CELL_HEIGHT/2+width/2} style={{stroke: '#666666', strokeWidth: width}} />
          <line x1={x2-CELL_WIDTH} y1={y} x2={x2} y2={y} style={{stroke: '#666666', strokeWidth: width}} />
        </Layer>
      ];
    } else {
      let x1 = (CELL_WIDTH+CELL_HGUTTER)*toX;
      let y = CELL_HEIGHT*this.props.data.y;
      let x2 = (CELL_WIDTH+CELL_HGUTTER)*fromX;
      return [
        <Layer key="beforeProcesses" name="beforeProcesses">
          <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#666666', strokeWidth: width}} />
        </Layer>,
        <Layer key="afterProcesses" name="afterProcesses">
          <line x1={x1+width/2} y1={y} x2={x1+width/2} y2={y+CELL_HEIGHT/2+width/2} style={{stroke: '#666666', strokeWidth: width}} />
          <line x1={x2+width/2} y1={y+width/2} x2={x2+width/2} y2={y-CELL_HEIGHT/2} style={{stroke: '#666666', strokeWidth: width}} />
          <line x1={x1} y1={y} x2={x1+CELL_WIDTH} y2={y} style={{stroke: '#666666', strokeWidth: width}} />
        </Layer>
      ];
    }
  }
}



class LinkElement extends React.Component {
  render() {
    let xs = [this.props.data.fromX, this.props.data.toX];
    xs.sort((a,b) => a-b);
    let x1 = (CELL_WIDTH+CELL_HGUTTER)*xs[0];
    let y = CELL_HEIGHT*this.props.data.y;
    let x2 = (CELL_WIDTH+CELL_HGUTTER)*xs[1] + CELL_WIDTH;
    return [
      <Layer key="beforeProcesses" name="beforeProcesses">
        <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#F2994A', strokeWidth: 2}} />
      </Layer>,
      <Layer key="afterProcesses" name="afterProcesses">
        <line x1={x1} y1={y} x2={x1+CELL_WIDTH} y2={y} style={{stroke: '#F2994A', strokeWidth: 2}} />
        <line x1={x2} y1={y} x2={x2-CELL_WIDTH} y2={y} style={{stroke: '#F2994A', strokeWidth: 2}} />
      </Layer>
    ];
  }
}



class MentionElement extends React.Component {
  render() {
    let xs = [this.props.data.fromX, this.props.data.toX];
    xs.sort((a,b) => a-b);
    let x1 = (CELL_WIDTH+CELL_HGUTTER)*xs[0];
    let y = CELL_HEIGHT*this.props.data.y;
    let x2 = (CELL_WIDTH+CELL_HGUTTER)*xs[1] + CELL_WIDTH;

    return <Layer name="beforeProcesses">
      <line x1={x1+CELL_WIDTH/2} y1={y} x2={x2} y2={y} style={{stroke: '#EDEDED', strokeWidth: 2}} />
    </Layer>;
  }
}



class PointElement extends React.Component {
  render() {
    let width = CELL_WIDTH+4;
    let height = 2;

    let x = Math.floor((CELL_WIDTH+CELL_HGUTTER)*this.props.data.x + (CELL_WIDTH - width)/2);
    let y = Math.floor(CELL_HEIGHT*this.props.data.y - height/2);
    return <Layer name="afterProcesses">
      <rect x={x} y={y} width={width} height={height} style={{fill: '#BDBDBD'}} />
    </Layer>;
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

  getChildContext() {
    return {getLayerNode: this.getLayerNode.bind(this)};
  }

  getLayerNode(key) {
    switch (key) {
    case 'beforeProcesses': return this.refs.beforeProcesses;
    case 'afterProcesses': return this.refs.afterProcesses;
    case 'processes': return this.refs.processes;
    }
    console.error("Unknown layer key: ", key);
    return null;
  }

  renderGrid() {
    // return null;
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

        {/* layers where dom is actually rendered */}
        <g ref="beforeProcesses"></g>
        <g ref="processes"></g>
        <g ref="afterProcesses"></g>
        

        {/* entities, compose different parts on different layers */}
        <g>
          {processes}
          {spawns}
          {links}
          {mentions}
          {points}
        </g>

      </SvgView>
      <ShellPanel width={SHELL_WIDTH} events={this.props.shellEvents} prompt={this.props.shellPrompt} submitInput={this.props.submitShellInput} />
    </div>;
  }
}
ScenarioView.childContextTypes = {
  getLayerNode: PropTypes.func
};

