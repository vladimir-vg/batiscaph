// pixels of empty space around figure that might be visible by dragging
V.WORKSPACE_PADDING = 100;



// utilitary function that splits given text into parts
// and uses giveb callback on each part
V.walkInfoText = (text, onText, onPid) => {
  let pidRe = /(<\d+\.\d+\.\d+>)/g;

  let prevIndex = 0;
  let match = pidRe.exec(text);
  while (match != null) {
    if (prevIndex != match.index) {
      onText(text.slice(prevIndex, match.index), prevIndex);
    }
    onPid(match[0], match.index);

    prevIndex = match.index + match[0].length;
    match = pidRe.exec(text);
  }

  if (prevIndex != text.length) {
    onText(text.slice(prevIndex, text.length), prevIndex);
  }
};



// some messages are shell-related and not really interesting for user
// this function tells them apart
V.isMessageValuable = (send, tree) => {
  let receiverAbsent = send.to && !(send.to in tree.procs);
  let senderToplevel = (send.from in tree.procs) && !(tree.procs[send.from].parent in tree.procs);
  let receiverToplevel = (send.to in tree.procs) && !(tree.procs[send.to].parent in tree.procs);

  if (receiverAbsent && send.term.indexOf("{io_request,") == 0) return false;
  if (send.toAtom == 'code_server' && send.term.indexOf("{code_call,") == 0) return false;
  if (senderToplevel && send.term.indexOf("{shell_cmd,") == 0) return false;
  if (receiverToplevel && send.term.indexOf("{shell_rep,") == 0) return false;

  return true;
}



class App extends React.Component {
  constructor() {
    super();
    this.state = {
      errorText: null,
      tree: null,
      hoveredItem: null,
      selectedItem: null,
      inputAllowed: false,
      shellStartAllowed: false,
      scenarios: null
    };
  }

  componentDidMount() {
    let hash = window.location.hash;

    V.socket = new WebSocket("ws://"+window.location.host+"/websocket");
    setTimeout(this.checkSocketState.bind(this), 1000);

    if (!hash) {
      this.requestScenarousList();
    } else if (hash.slice(0,2) == '#/') {
      let url = '/scenarios/' + hash.slice(2);
      this.fetchCsvAndLoad(url);
    } else {
      this.setState({errorText: 'Wrong '+hash+' hash address'});
    }
  }

  checkSocketState() {
    if (V.socket.readyState == WebSocket.OPEN) {
      this.setState({shellStartAllowed: true});
    }
  }

  requestScenarousList() {
    fetch('/scenarios.json').then((response) => {
      response.json().then((items) => {
        this.setState({scenarios: items});
      });
    });
  }

  startNewShell() {
    V.socket.onmessage = (function (event) {
      if (event.data.slice(0,7) == "events ") {
        let rows = JSON.parse(event.data.slice(7));
        let tree = V.processEvents(this.state.tree, rows, 'json');
        this.setState({tree: tree, inputAllowed: true});
      } else if (event.data.slice(0,14) == "shell_started ") {
        let path = event.data.slice(14);
        window.location.hash = "/" + path;
      }
    }).bind(this);

    V.socket.send("start_shell");
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

  onItemSelect(item) {
    if (!item) {
      this.setState({selectedItem: null});
    } else if (this.state.selectedItem && this.state.selectedItem.key == item.key && this.state.selectedItem.type == item.type) {
      // toggle in case of same item select
      this.setState({selectedItem: null});
    } else {
      // if selected pid that not present on page then do nothing
      // for example parent of top process
      if (item.type == 'proc' && !(item.key in this.state.tree.procs)) {
        return;
      }
      this.setState({selectedItem: item});
    }
  }

  onItemHover(item) {
    if (!item) {
      this.setState({hoveredItem: null});
    } else {
      this.setState({hoveredItem: item});
    }
  }

  onShellInput(text) {
    V.socket.send("shell_input "+text+"\n");
  }

  onRestartRequest() {
    V.socket.send("shell_restart");
  }

  onModuleStore(name, body) {
    V.socket.send("store_module " + name + "\n" + body);
  }

  renderMainPage() {
    let shellBlock = null;
    if (this.state.shellStartAllowed) {
      shellBlock = <div className="head-block">
        <button className="btn" onClick={this.startNewShell.bind(this)}>Start new shell</button>
      </div>;
    }
    return <div className="content-page">
      {shellBlock}
      <ScenariosList scenarios={this.state.scenarios} />
    </div>;
  }

  render() {
    if (this.state.errorText) {
      return <div>{this.state.errorText}</div>;
    }

    if (!window.location.hash) {
      if (!this.state.scenarios) {
        return <div className="content-page">Loading scenarios...</div>;
      } else {
        return this.renderMainPage();
      }
    }

    let inputPanel = null;
    if (this.state.tree && this.state.inputAllowed) {
      inputPanel = <InputPanel tree={this.state.tree} storeModule={this.onModuleStore.bind(this)}
        submitInput={this.onShellInput.bind(this)} requestRestart={this.onRestartRequest.bind(this)} />;
    }

    if (this.state.tree) {
      let paddedWidth = this.state.tree.maxX*(V.CELL_WIDTH + V.CELL_GUTTER);
      let paddedHeight = this.state.tree.maxY*V.CELL_HEIGHT;
      return <div className="container">
        <SvgView className="svg-area" padding={V.WORKSPACE_PADDING} paddedWidth={paddedWidth} paddedHeight={paddedHeight}>
          <HoverSelection tree={this.state.tree} selectedItem={this.state.selectedItem} hoveredItem={this.state.hoveredItem}
            onItemSelect={this.onItemSelect.bind(this)} onItemHover={this.onItemHover.bind(this)} />
          <ProcessTreeView tree={this.state.tree} selectedItem={this.state.selectedItem} hoveredItem={this.state.hoveredItem}
            onItemSelect={this.onItemSelect.bind(this)} onItemHover={this.onItemHover.bind(this)} />
          <ShellIOView tree={this.state.tree} width={paddedWidth}
            selectedItem={this.state.selectedItem} hoveredItem={this.state.hoveredItem}
            onItemSelect={this.onItemSelect.bind(this)} onItemHover={this.onItemHover.bind(this)} />
          <MessageSends tree={this.state.tree} selectedItem={this.state.selectedItem} hoveredItem={this.state.hoveredItem}
            onItemSelect={this.onItemSelect.bind(this)} onItemHover={this.onItemHover.bind(this)} />
        </SvgView>
        <div className="aside-area">
          <SelectedItemInfo tree={this.state.tree}
            selectedItem={this.state.selectedItem} hoveredItem={this.state.hoveredItem}
            onItemSelect={this.onItemSelect.bind(this)} onItemHover={this.onItemHover.bind(this)} />
        </div>

        {inputPanel}
      </div>;
    }

    return <div className="content-page">Loading...</div>;
  }
}



document.addEventListener("DOMContentLoaded", function(event) {
  ReactDOM.render(<App />, document.getElementById('react-app'));
});
