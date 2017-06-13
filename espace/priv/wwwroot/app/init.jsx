// pixels of empty space around figure that might be visible by dragging
V.WORKSPACE_PADDING = 100;



class App extends React.Component {
  constructor() {
    super();
    this.state = {
      errorText: null,
      tree: null,
      selectedPid: null
    };
  }

  componentDidMount() {
    let hash = window.location.hash;

    if (!hash) {
      this.startNewShell();
    } else if (hash.slice(0,2) == '#/') {
      let url = '/scenarios/' + hash.slice(2);
      this.fetchCsvAndLoad(url);
    } else {
      this.setState({errorText: 'Wrong '+hash+' hash address'});
    }
  }

  startNewShell() {
    V.socket = new WebSocket("ws://"+window.location.host+"/websocket");
    V.socket.onmessage = (function (event) {
      if (event.data.slice(0,7) == "events ") {
        let rows = JSON.parse(event.data.slice(7));
        let tree = V.processEvents(this.state.tree, rows, 'json');
        this.setState({tree: tree});
      } else if (event.data.slice(0,14) == "shell_started ") {
        let path = event.data.slice(14);
        window.location.hash = "/" + path;
      }
    }).bind(this);
    V.socket.onopen = function (event) {
      V.socket.send("start_shell");
    };
  }

  fetchCsvAndLoad(url) {
    fetch(url).then((response) => {
      response.text().then((text) => {
        let rows = CSV.parse(text);
        let keys = rows.shift();
        let tree = V.processEvents(undefined, rows, keys);
        this.setState({tree: tree});
      });
    });
  }

  onProcSelect(pid) {
    this.setState({selectedPid: pid});
  }

  render() {
    if (this.state.errorText) {
      return <div>{this.state.errorText}</div>;
    }

    if (this.state.tree) {
      let paddedWidth = this.state.tree.maxX*(V.CELL_WIDTH + V.CELL_GUTTER);
      let paddedHeight = this.state.tree.maxY*V.CELL_HEIGHT;
      return <div>
        <SvgView padding={V.WORKSPACE_PADDING} paddedWidth={paddedWidth} paddedHeight={paddedHeight}>
          <ProcessTreeView tree={this.state.tree} selectedPid={this.state.selectedPid} onProcSelect={this.onProcSelect.bind(this)} />
          <ShellIOView tree={this.state.tree} width={paddedWidth} />
        </SvgView>
        <SelectedProcInfo tree={this.state.tree} pid={this.state.selectedPid} />
      </div>;
    }

    return <div>Loading...</div>;
  }
}



document.addEventListener("DOMContentLoaded", function(event) {
  ReactDOM.render(<App />, document.getElementById('react-app'));
});
