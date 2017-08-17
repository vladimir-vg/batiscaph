
const CELL_WIDTH2 = 8;
const CELL_HEIGHT2 = 10;
const CELL_HGUTTER2 = 5;
// const CELL_VGUTTER2 = 2;

const Route = ReactRouterDOM.Route;

class ProcessElement extends React.Component {
  renderTracedPart(part, i) {
    let x = (CELL_WIDTH2+CELL_HGUTTER2)*this.props.data.x;
    let y = CELL_HEIGHT2*part.fromY;
    let width = CELL_WIDTH2;
    let height = CELL_HEIGHT2*(part.toY - part.fromY);
    return <rect key={i} x={x} y={y} width={width} height={height} style={{fill: '#D9D9D9'}} />;
  }

  render() {
    let nodes = [];
    for (var i in this.props.data.parts) {
      let part = this.props.data.parts[i];
      if (part.type == 'TRACED') {
        nodes.push(this.renderTracedPart(part, i));
      }
    }
    return <g>
      {nodes}
    </g>;
  }
}

class SpawnElement extends React.Component {
  render() {
    return null;
  }
}

class LinkElement extends React.Component {
  render() {
    return null;
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

    return <svg width={600} height={600} style={{marginTop: 10, marginLeft: 10}}>
      <g>{processes}</g>
      <g>{spawns}</g>
      <g>{links}</g>
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