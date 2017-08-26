
const CELL_WIDTH2 = 10;
const CELL_HEIGHT2 = 10;
const CELL_HGUTTER2 = 5;
// const CELL_VGUTTER2 = 2;

const MENTION_PADDING = 2;

const Route = ReactRouterDOM.Route;
const Link = ReactRouterDOM.Link;
const Redirect = ReactRouterDOM.Redirect;

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

  renderDeadPart(part, i) {
    let crossSize = Math.min(CELL_WIDTH2, CELL_HEIGHT2);

    let x = (CELL_WIDTH2+CELL_HGUTTER2)*this.props.data.x;
    let y = CELL_HEIGHT2*part.y - CELL_HEIGHT2/2;

    let x1 = x+(CELL_WIDTH2-crossSize)/2;
    let y1 = y+(CELL_HEIGHT2-crossSize)/2;
    let x2 = x+CELL_WIDTH2-(CELL_WIDTH2-crossSize)/2;
    let y2 = y+CELL_HEIGHT2-(CELL_HEIGHT2-crossSize)/2;

    return <g key={i}>
      <line x1={x1} y1={y1} x2={x2} y2={y2} style={{stroke: '#FC8888', strokeWidth: 2}} />
      <line x1={x2} y1={y1} x2={x1} y2={y2} style={{stroke: '#FC8888', strokeWidth: 2}} />
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
      } else if (part.type == 'DEAD') {
        nodes.push(this.renderDeadPart(part, i));
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
  componentDidMount() {
    this.props.onInstanceIdChange(this.props.match.params.id);
  }

  renderGrid() {
    let lines = [];
    for (let i = -10; i < 100; i++) {
      let xa = i*(CELL_WIDTH2+CELL_HGUTTER2);
      let xb = i*(CELL_WIDTH2+CELL_HGUTTER2) + CELL_WIDTH2;
      lines.push(<line key={'ha-' + i} x1={xa+0.5} y1={-100} x2={xa+0.5} y2={2000} style={{stroke: '#fee'}} />);
      lines.push(<line key={'hb-' + i} x1={xb-0.5} y1={-100} x2={xb-0.5} y2={2000} style={{stroke: '#fee'}} />);
    }
    for (let i = -10; i < 100; i++) {
      let y = i*CELL_HEIGHT2;
      lines.push(<line key={'v-' + i} x1={-100} y1={y+0.5} x2={2000} y2={y+0.5} style={{stroke: '#fee'}} />);
    }
    return lines;
  }

  render() {
    if (!this.props.tree) { return <div />; }

    let processes = [], spawns = [], links = [];
    for (let pid in this.props.tree.processes) {
      processes.push(<ProcessElement key={pid} data={this.props.tree.processes[pid]} />);
    }
    for (let key in this.props.tree.spawns) {
      processes.push(<SpawnElement key={key} data={this.props.tree.spawns[key]} />);
    }
    for (let key in this.props.tree.links) {
      processes.push(<LinkElement key={key} data={this.props.tree.links[key]} />);
    }

    return <SvgView2 padding={100} paddedWidth={1000} paddedHeight={1000}>
      <g>{this.renderGrid()}</g>
      <g>{processes}</g>
      <g>{spawns}</g>
      <g>{links}</g>
    </SvgView2>;
  }
}



class MainPage extends React.Component {
  constructor() {
    super();
    this.state = {
      items: []
    };
  }

  componentDidMount() {
    this.requestScenarousList();
  }

  requestScenarousList() {
    fetch('/scenarios.json').then((response) => {
      response.json().then((items) => {
        this.setState({items: items});
      });
    });
  }

  render() {
    let links = [];
    for (const i in this.state.items) {
      let id = this.state.items[i].instance_id;
      links.push(<div key={i}><Link to={'/scenarios2/'+id} className="item">{id}</Link></div>);
    }

    return <div className="content-page">
      <div className="head-block">
        <button className="btn" onClick={this.props.startNewShell}>Start new shell</button>
      </div>
      <div className="scenarios-list-block">
        {links}
      </div>
    </div>;
  }
}



class App2 extends React.Component {
  constructor() {
    super();
    this.state = {
      tree: undefined,
      ongoingScenario: false // if active, then shell input/output is possible
    };
    this._layout = {};

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.startNewShell = this.startNewShell.bind(this);
    this.onInstanceIdChange = this.onInstanceIdChange.bind(this);
    this.onWSMessage = this.onWSMessage.bind(this);
  }

  onInstanceIdChange(id) {
    this.fetchInitialDelta(id);
  }

  fetchInitialDelta(id) {
    let url = '/api/scenarios2/' + id;
    fetch(url).then((response) => {
      response.json().then((delta) => {
        this.applyDeltaSetState(delta);
      });
    });
  }

  onWSMessage(event) {
    if (event.data.slice(0,14) == "shell_started ") {
      let id = event.data.slice(14);
      this.setState({instanceId: id});
    } else if (event.data.slice(0,6) == 'delta ') {
      let delta = JSON.parse(event.data.slice(6));
      this.applyDeltaSetState(delta);
    }
  }

  applyDeltaSetState(delta) {
    V.updateLayout(delta, this._layout);
    let tree = V.produceTree(this._layout);
    console.log("layout", this._layout);
    console.log("tree", tree);
    this.setState({tree: tree});
  }

  startNewShell() {
    this.setState({ongoingScenario: true});

    V.socket = new WebSocket("ws://"+window.location.host+"/websocket");
    V.socket.addEventListener('message', this.onWSMessage);
    V.socket.addEventListener('open', function () {
      V.socket.send('start_shell');
    });
  }

  render() {
    let scenarioRedirect = null;
    if (this.state.instanceId) {
      scenarioRedirect = <Redirect to={"/scenarios2/" + this.state.instanceId} />;
    }

    return <div>
      <Route path="/" exact={true} render={(props) => <MainPage startNewShell={this.startNewShell} />} />
      <Route path="/scenarios2/:id" render={(props) => <ScenarioView2 tree={this.state.tree} onInstanceIdChange={this.onInstanceIdChange} {...props} />} />
      {scenarioRedirect}
    </div>;
  }
};