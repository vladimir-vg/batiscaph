const OFFER_TO_RESTART_AFTER = 3000;



class ErlFileInput extends React.Component {
  onChange() {
    let hash = window.location.hash;

    if (hash.slice(0,2) != '#/') {
      console.error("expected to have scenario hash address")
      return;
    }

    let url = '/scenarios/' + hash.slice(2) + "/erlmodules";
    fetch(url, {
      method: 'POST',
      body: new FormData(this.refs.form)
    }).then(() => {
      this.refs.form.reset();
    }).catch((error) => {
      this.refs.form.reset();
    });
  }

  render() {
    return <form ref="form" className="notice-text">
      upload *.erl module: <input type="file" name="modules" accept=".erl" multiple={true} onChange={this.onChange.bind(this)} />
    </form>;
  }
}



class InputPanel extends React.Component {
  constructor() {
    super();
    this.state = {
      text: "",
      showRestartButton: false,
      restartRequested: false,
      moduleName: null
    };
  }

  componentWillReceiveProps(props) {
    if (props.tree.currentPrompt != this.props.tree.currentPrompt) {
      // unlock restart request after prompt became alive again
      this.setState({restartRequested: false, showRestartButton: false, moduleName: null, text: ""});
    }
  }

  showRestartButton() {
    if (this._showRequestRestartTimeout) clearTimeout(this._showRequestRestartTimeout);
    this._showRequestRestartTimeout = null;

    // if shell is already alive, then just ignore this
    if (this.props.tree.currentPrompt) return;

    this.setState({showRestartButton: true});
  }

  requestRestart() {
    this.props.requestRestart();
    this.setState({restartRequested: true});
  }

  onTextChange(e) {
    let text = e.target.value;
    let match = /-module\(([a-zA-Z-0-9_]+)\)/.exec(text);
    let moduleName = false;
    if (match && match[1]) {
      moduleName = match[1];
    }
    this.setState({text: e.target.value, moduleName: moduleName});
  }

  sendToShell() {
    if (this._showRequestRestartTimeout) clearTimeout(this._showRequestRestartTimeout);
    this._showRequestRestartTimeout = setTimeout(this.showRestartButton.bind(this), OFFER_TO_RESTART_AFTER);

    if (this.state.moduleName) {
      this.props.storeModule(this.state.moduleName, this.state.text);
      this.setState({text: "", moduleName: null});
    } else {
      this.props.submitInput(this.state.text);
      this.setState({text: ""});
    }
  }

  onKeyPress(e) {
    // plain Enter makes new line
    // Shift + Enter sends input to remote shell
    if (e.key === 'Enter' && e.shiftKey && this.state.text) {
      this.sendToShell()
    }
  }

  render() {
    let prompt = "...";
    let inputDisabled = true;
    let promptStyle = {
      width: V.SHELL_SIDELINE_WIDTH,
      marginLeft: V.WORKSPACE_PADDING-V.SHELL_SIDELINE_WIDTH,
      textAlign: 'center'
    };
    if (this.props.tree.currentPrompt) {
      prompt = this.props.tree.currentPrompt;
      inputDisabled = null;
      delete promptStyle.textAlign;
    }

    let buttonSendText = "send to shell";
    let moduleBlock = null;
    if (this.state.moduleName) {
      moduleBlock = <div className="notice-text">will be sent as: <strong>{this.state.moduleName}.erl</strong></div>
      buttonSendText = "store erlang module"
    } else {
      moduleBlock = <ErlFileInput />;
    }

    let content = <div>
      <textarea rows="3" disabled={inputDisabled} value={this.state.text}
        onChange={this.onTextChange.bind(this)} onKeyPress={this.onKeyPress.bind(this)} />
      {moduleBlock}
    </div>;

    if (this.state.showRestartButton) {
      let button = <button onClick={this.requestRestart.bind(this)} disabled={this.state.restartRequested}>restart</button>;
      content = <div className="notice-text">
        You can {button} shell if execution takes too long
      </div>;
    }

    return <div className="input-panel">
      <div style={promptStyle} className="prompt">
        {prompt}
      </div>
      <div className="input">
        {content}
      </div>
      <div className="info notice-text">
        use Enter to make a new line <br />
        and Shift + Enter to send input to remote shell <br />
        <button onClick={this.sendToShell.bind(this)} disabled={inputDisabled}>{buttonSendText}</button>
      </div>
    </div>;
  }
};

InputPanel.propTypes = {
  tree: React.PropTypes.object.isRequired,
  submitInput: React.PropTypes.func.isRequired,
  storeModule: React.PropTypes.func.isRequired,
  requestRestart: React.PropTypes.func.isRequired
};