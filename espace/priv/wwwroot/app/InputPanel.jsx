class InputPanel extends React.Component {
  constructor() {
    super();
    this.state = {
      text: ""
    };
  }

  onSubmit(e) {
    e.preventDefault();

  }

  onTextChange(e) {
    this.setState({text: e.target.value});
  }

  onKeyPress(e) {
    // Shift + Enter makes new line
    // plain Enter sends input to remote shell
    if (e.key === 'Enter' && !e.shiftKey && this.state.text) {
      this.props.onInput(this.state.text);
      this.setState({text: ""});
    }
  }

  render() {
    let prompt = "...";
    let disabled = true;
    if (this.props.tree.currentPrompt) {
      prompt = this.props.tree.currentPrompt;
      disabled = null;
    }

    return <div className="input-panel">
      <div style={{width: V.SHELL_SIDELINE_WIDTH, marginLeft: V.WORKSPACE_PADDING-V.SHELL_SIDELINE_WIDTH}} className="prompt">
        {prompt}
      </div>
      <div className="input">
        <textarea disabled={disabled} value={this.state.text}
            onChange={this.onTextChange.bind(this)} onKeyPress={this.onKeyPress.bind(this)} />
      </div>
      <div className="info">
        use Enter to send input to remote shell<br />
        and Shift + Enter to make a new line
      </div>
    </div>;
  }
};

InputPanel.propTypes = {
  tree: React.PropTypes.object.isRequired,
  onInput: React.PropTypes.func.isRequired
};