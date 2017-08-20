
const CELL_WIDTH2 = 10;
const CELL_HEIGHT2 = 10;
const CELL_HGUTTER2 = 5;
// const CELL_VGUTTER2 = 2;

const MENTION_PADDING = 2;

const Route = ReactRouterDOM.Route;

class ProcessElement extends React.Component {
  renderTracedPart(part, i) {
    let x = (CELL_WIDTH2+CELL_HGUTTER2)*this.props.data.x;
    let y = CELL_HEIGHT2*part.fromY;
    let width = CELL_WIDTH2;
    let height = CELL_HEIGHT2*(part.toY - part.fromY);

    let untracedNode = null;
    if (part.untracedHead) {
      let mX = x + MENTION_PADDING;
      let mWidth = width - 2*MENTION_PADDING;
      untracedNode = <g>
        <rect x={x} y={y-CELL_HEIGHT2/2} width={width} height={CELL_HEIGHT2/2} style={{fill: '#D9D9D9'}} />
        <rect x={mX} y={y-CELL_HEIGHT2/2} width={mWidth} height={CELL_HEIGHT2/2} style={{fill: '#EDEDED'}} />
      </g>;
    }

    let exitRect = null;
    if (part.exitMark) {
      let h = part.unnormalExit ? (CELL_HEIGHT2/2) : 2;
      exitRect = <rect x={x} y={y+height-h} width={width} height={h} style={{fill: '#FC8888'}} />
    }
    return <g key={i}>
      {untracedNode}
      <rect x={x} y={y} width={width} height={height} style={{fill: '#D9D9D9'}} />
      {exitRect}
    </g>;
  }

  renderMentionPart(part, i) {
    let x = (CELL_WIDTH2+CELL_HGUTTER2)*this.props.data.x;
    let y = CELL_HEIGHT2*part.y - Math.floor(CELL_HEIGHT2/2);
    let width = CELL_WIDTH2;
    let height = CELL_HEIGHT2;

    let mX = x + MENTION_PADDING;
    let mWidth = width - 2*MENTION_PADDING;

    return <g key={i}>
      <rect x={x} y={y} width={width} height={height} style={{fill: '#D9D9D9'}} />
      <rect x={mX} y={y} width={mWidth} height={height} style={{fill: '#EDEDED'}} />
    </g>;
  }

  render() {
    let nodes = [];
    for (const i in this.props.data.parts) {
      let part = this.props.data.parts[i];
      if (part.type == 'TRACED') {
        nodes.push(this.renderTracedPart(part, i));
      } else if (part.type == 'MENTION') {
        nodes.push(this.renderMentionPart(part, i));
      }
    }
    return <g>
      {nodes}
    </g>;
  }
}

class SpawnElement extends React.Component {
  render() {
    let width = 2;
    let fromX = this.props.data.fromX, toX = this.props.data.toX;
    if (fromX < toX) {
      let x1 = (CELL_WIDTH2+CELL_HGUTTER2)*fromX + CELL_WIDTH2;
      let y = CELL_HEIGHT2*this.props.data.y;
      let x2 = (CELL_WIDTH2+CELL_HGUTTER2)*toX + CELL_WIDTH2;
      return <g>
        <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#666666', strokeWidth: width}} />
        <line x1={x1-width/2} y1={y+width/2} x2={x1-width/2} y2={y-CELL_HEIGHT2/2} style={{stroke: '#666666', strokeWidth: width}} />
        <line x1={x2-width/2} y1={y} x2={x2-width/2} y2={y+CELL_HEIGHT2/2+width/2} style={{stroke: '#666666', strokeWidth: width}} />
      </g>;
    } else {
      let x1 = (CELL_WIDTH2+CELL_HGUTTER2)*toX;
      let y = CELL_HEIGHT2*this.props.data.y;
      let x2 = (CELL_WIDTH2+CELL_HGUTTER2)*fromX;
      return <g>
        <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#666666', strokeWidth: width}} />
        <line x1={x1+width/2} y1={y} x2={x1+width/2} y2={y+CELL_HEIGHT2/2+width/2} style={{stroke: '#666666', strokeWidth: width}} />
        <line x1={x2+width/2} y1={y+width/2} x2={x2+width/2} y2={y-CELL_HEIGHT2/2} style={{stroke: '#666666', strokeWidth: width}} />
      </g>;
    }
  }
}

class LinkElement extends React.Component {
  render() {
    let xs = [this.props.data.fromX, this.props.data.toX];
    xs.sort();
    let x1 = (CELL_WIDTH2+CELL_HGUTTER2)*xs[0];
    let y = CELL_HEIGHT2*this.props.data.y;
    let x2 = (CELL_WIDTH2+CELL_HGUTTER2)*xs[1] + CELL_WIDTH2;
    return <g>
      <line x1={x1} y1={y} x2={x2} y2={y} style={{stroke: '#F2994A', strokeWidth: 2}} />
    </g>;
  }
}



class ScenarioView2 extends React.Component {
  constructor() {
    super();
    this.state = {
      tree: undefined
    };
    this._layout = {};
  }

  componentDidMount() {
    let url = '/api/scenarios2/' + this.props.match.params.id;
    this.fetchInitialTree(url);
  }

  fetchInitialTree(url) {
    fetch(url).then((response) => {
      response.json().then((delta) => {
        V.updateLayout(delta, this._layout);
        let tree = V.produceTree(this._layout);
        console.log("layout", this._layout);
        console.log("tree", tree);
        this.setState({tree: tree});
      });
    });
  }

  render() {
    if (!this.state.tree) { return <div />; }

    let processes = [], spawns = [], links = [];
    for (let pid in this.state.tree.processes) {
      processes.push(<ProcessElement key={pid} data={this.state.tree.processes[pid]} />);
    }
    for (let key in this.state.tree.spawns) {
      processes.push(<SpawnElement key={key} data={this.state.tree.spawns[key]} />);
    }
    for (let key in this.state.tree.links) {
      processes.push(<LinkElement key={key} data={this.state.tree.links[key]} />);
    }

    let gridLines = [];
    for (let i = -10; i < 100; i++) {
      let xa = i*(CELL_WIDTH2+CELL_HGUTTER2);
      let xb = i*(CELL_WIDTH2+CELL_HGUTTER2) + CELL_WIDTH2;
      gridLines.push(<line key={'ha-' + i} x1={xa+0.5} y1={-100} x2={xa+0.5} y2={1000} style={{stroke: '#ffdbdb'}} />);
      gridLines.push(<line key={'hb-' + i} x1={xb-0.5} y1={-100} x2={xb-0.5} y2={1000} style={{stroke: '#ffdbdb'}} />);
    }
    for (let i = -10; i < 100; i++) {
      let y = i*CELL_HEIGHT2;
      gridLines.push(<line key={'v-' + i} x1={-100} y1={y+0.5} x2={1000} y2={y+0.5} style={{stroke: '#ffdbdb'}} />);
    }

    return <svg width={700} height={700}>
      <g transform={"translate(50,50)"}>
        <g>{gridLines}</g>
        <g>{processes}</g>
        <g>{spawns}</g>
        <g>{links}</g>
      </g>
    </svg>;
  }
}

class App2 extends React.Component {
  constructor() {
    super();
    this.state = {
    };

    // avoid repeating .bind(this) by doing it once
  }

  render() {
    return <div>
      <Route path="/scenarios2/:id" component={ScenarioView2} />
    </div>;
  }
};