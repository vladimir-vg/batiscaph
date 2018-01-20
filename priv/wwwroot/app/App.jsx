const Route = ReactRouterDOM.Route;
const Redirect = ReactRouterDOM.Redirect;



class App extends React.Component {
  constructor() {
    super();
    this.state = {
      instanceId: undefined,
      tree: undefined,
      shellPrompt: undefined,
      shellEvents: [],
      selectedContext: null
    };
    this._layout = {};

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.startNewShell = this.startNewShell.bind(this);
    this.onInstanceIdChange = this.onInstanceIdChange.bind(this);
    this.onWSMessage = this.onWSMessage.bind(this);
    this.submitShellInput = this.submitShellInput.bind(this);
    this.tracePid = this.tracePid.bind(this);
    this.selectContext = this.selectContext.bind(this);
  }

  onInstanceIdChange(id, context) {
    this._layout = {};
    this.setState({tree: undefined, shellPrompt: undefined, shellEvents: []});
    if (V.socket) {
      V.socket.close();
      V.socket = null;
    }
    this.connectToExistingShell(id, context);
    // this.fetchInitialDelta(id);
  }

  onWSMessage(event) {
    if (event.data.indexOf("shell_connected ") == 0) {
      let id = event.data.slice("shell_connected ".length);
      this.setState({instanceId: id});
    } else if (event.data.indexOf("delta ") == 0) {
      let delta = JSON.parse(event.data.slice("delta ".length));
      console.log("delta", delta);
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
    for (let i in delta.events) {
      let e = delta.events[i];
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

  startNewShell(id) {
    V.socket = new WebSocket("ws://"+window.location.host+"/websocket");
    V.socket.addEventListener('message', this.onWSMessage);
    V.socket.addEventListener('open', (function () {
      V.socket.send('start_shell');
    }).bind(this));
  }

  connectToExistingShell(id, context) {
    V.socket = new WebSocket("ws://"+window.location.host+"/websocket");
    V.socket.addEventListener('message', this.onWSMessage);
    let cmd = 'connect_to_shell ' + id;
    if (context) {
      cmd += " " + context;
    }
    V.socket.addEventListener('open', (function () {
      V.socket.send(cmd);
    }).bind(this));
  }

  submitShellInput(text) {
    V.socket.send('shell_input ' + text + "\n");
  }

  tracePid(pid) {
    if (!pid) { console.error("Got request to trace invalid pid:", pid); return; }
    V.socket.send('trace_pid ' + pid);
  }

  selectContext(key) {
    this.setState({selectedContext: key});
  }

  render() {
    let scenarioRedirect = null;
    if (this.state.instanceId) {
      scenarioRedirect = <Redirect to={"/scenarios/" + this.state.instanceId} />;
    }

    return <div>
      <Route path="/" exact={true} render={(props) => <MainPage startNewShell={this.startNewShell} />} />
      <Route path="/scenarios/:id/:context*" render={(props) =>
          <ScenarioView tree={this.state.tree} shellPrompt={this.state.shellPrompt} shellEvents={this.state.shellEvents}
            submitShellInput={this.submitShellInput} onInstanceIdChange={this.onInstanceIdChange}
            tracePid={this.tracePid} selectContext={this.selectContext}
            selectedContext={this.state.selectedContext}
            {...props} />
      } />
      {scenarioRedirect}
    </div>;
  }
};
