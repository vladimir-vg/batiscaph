


const Route = ReactRouterDOM.Route;
const Link = ReactRouterDOM.Link;
const Redirect = ReactRouterDOM.Redirect;



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



class App extends React.Component {
  constructor() {
    super();
    this.state = {
      tree: undefined,
      shellInfo: undefined,
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
    this._layout = {};
    this.setState({tree: undefined});
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
      <Route path="/scenarios2/:id" render={(props) => <ScenarioView tree={this.state.tree} shellInfo={this.state.shellInfo} onInstanceIdChange={this.onInstanceIdChange} {...props} />} />
      {scenarioRedirect}
    </div>;
  }
};
