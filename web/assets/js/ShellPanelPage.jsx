import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';
import PropTypes from 'prop-types';



class Command extends React.Component {
  render() {
    return <div className="Command">
      <div className="input-container">
        <div className="prompt noselect">{this.props.cmd.Prompt}</div>
        <div className="input">{this.props.cmd.Input}</div>
      </div>
      <div className="outputs">
        {this.props.cmd.Outputs.map(({ At, Text }) => <pre key={At}>{Text}</pre>)}
      </div>
    </div>;
    return null;
  }
}
Command.propTypes = {
  cmd: PropTypes.object.isRequired,
}



@inject("store") @observer
export default class ShellPanelPage extends React.Component {
  constructor() {
    super();

    this.onInputChange = this.onInputChange.bind(this);
    this.submitInput = this.submitInput.bind(this);

    this.state = {
      text: ''
    }
  }

  componentDidMount() {
    this.props.store.ensureShellConnected();
  }

  onInputChange(e) {
    this.setState({text: e.target.value});
  }

  submitInput() {
    console.log(this.state.text);
    this.props.store.sendShellInput(this.state.text);
    this.setState({text: ''});
  }

  renderShellCommands() {
    const result = [];

    const cmds = this.props.store.shellCommands;
    for (const i in cmds) {
      const cmd = cmds[i];
      result.push(<Command key={i} cmd={cmds[i]} />);
    }

    return result;
  }

  render() {
    return <div className="ShellPanel">
      <div>{this.renderShellCommands()}</div>
      <div className="current-input">
        <textarea value={this.state.text} onChange={this.onInputChange} />
        <button onClick={this.submitInput}>Submit</button>
      </div>
    </div>;
  }
}
