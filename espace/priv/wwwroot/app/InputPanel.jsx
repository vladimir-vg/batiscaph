const OFFER_TO_RESTART_AFTER = 3000;



class InputPanel extends React.Component {
  constructor() {
    super();
    this.state = {
      text: "",
      showRestartButton: false,
      restartRequested: false
    };
  }

  componentWillReceiveProps(props) {
    if (props.tree.currentPrompt) {
      // unlock restart request after prompt became alive again
      this.setState({restartRequested: false, showRestartButton: false});
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
    this.setState({text: e.target.value});
  }

  onKeyPress(e) {
    // Shift + Enter makes new line
    // plain Enter sends input to remote shell
    if (e.key === 'Enter' && !e.shiftKey && this.state.text) {
      if (this._showRequestRestartTimeout) clearTimeout(this._showRequestRestartTimeout);
      this._showRequestRestartTimeout = setTimeout(this.showRestartButton.bind(this), OFFER_TO_RESTART_AFTER);

      this.props.submitInput(this.state.text);
      this.setState({text: ""});
    }
  }

  render() {
    let prompt = "...";
    let disabled = true;
    let promptStyle = {
      width: V.SHELL_SIDELINE_WIDTH,
      marginLeft: V.WORKSPACE_PADDING-V.SHELL_SIDELINE_WIDTH,
      textAlign: 'center'
    };
    if (this.props.tree.currentPrompt) {
      prompt = this.props.tree.currentPrompt;
      disabled = null;
      delete promptStyle.textAlign;
    }

    let content = <textarea disabled={disabled} value={this.state.text}
        onChange={this.onTextChange.bind(this)} onKeyPress={this.onKeyPress.bind(this)} />;

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
        use Enter to send input to remote shell<br />
        and Shift + Enter to make a new line
      </div>
    </div>;
  }
};

InputPanel.propTypes = {
  tree: React.PropTypes.object.isRequired,
  submitInput: React.PropTypes.func.isRequired,
  requestRestart: React.PropTypes.func.isRequired
};