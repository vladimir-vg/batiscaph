


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
      shellPrompt: undefined,
      shellEvents: [],
      ongoingScenario: false // if active, then shell input/output is possible
    };
    this._layout = {};

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.startNewShell = this.startNewShell.bind(this);
    this.onInstanceIdChange = this.onInstanceIdChange.bind(this);
    this.onWSMessage = this.onWSMessage.bind(this);
    this.submitShellInput = this.submitShellInput.bind(this);
  }

  onInstanceIdChange(id) {
    this._layout = {};
    this.setState({tree: undefined, shellPrompt: undefined, shellEvents: []});
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
    } else if (event.data.indexOf('shell_input_ready ') == 0) {
      let prompt = event.data.split(' ')[1];
      this.updateShellReady(prompt);
    } else if (event.data == 'shell_input_stopped') {
      this.updateShellReady(null);
    }
  }

  updateShellReady(prompt) {
    this.setState({shellPrompt: prompt});
  }

  produceShellEvents(delta) {
    let events = this.state.shellEvents.slice();
    for (let i in delta.table_events) {
      let e = delta.table_events[i];
      if (e.type == 'shell_input_expected') {
      } else if (e.type == 'shell_input') {
        events.push(e);
      } else if (e.type == 'shell_output') {
        events.push(e);
      }
    }
    return events;
  }

  applyDeltaSetState(delta) {
    V.updateLayout(delta, this._layout);
    let tree = V.produceTree(this._layout);
    let shellEvents = this.produceShellEvents(delta);
    console.log("layout", this._layout);
    console.log("tree", tree);
    console.log("shellEvents", shellEvents);
    this.setState({tree: tree, shellEvents: shellEvents});
  }

  startNewShell() {
    this.setState({ongoingScenario: true});

    V.socket = new WebSocket("ws://"+window.location.host+"/websocket");
    V.socket.addEventListener('message', this.onWSMessage);
    V.socket.addEventListener('open', function () {
      V.socket.send('start_shell');
    });
  }

  submitShellInput(text) {
    V.socket.send('shell_input ' + text + "\n");
  }

  render() {
    let scenarioRedirect = null;
    if (this.state.instanceId) {
      scenarioRedirect = <Redirect to={"/scenarios2/" + this.state.instanceId} />;
    }

    return <div>
      <Route path="/" exact={true} render={(props) => <MainPage startNewShell={this.startNewShell} />} />
      <Route path="/scenarios2/:id" render={(props) => <ScenarioView tree={this.state.tree} shellPrompt={this.state.shellPrompt} shellEvents={this.state.shellEvents} submitShellInput={this.submitShellInput} onInstanceIdChange={this.onInstanceIdChange} {...props} />} />
      {scenarioRedirect}
    </div>;
  }
};
