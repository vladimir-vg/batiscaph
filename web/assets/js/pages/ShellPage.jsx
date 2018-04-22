import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import React from 'react';
import { Link } from 'react-router-dom';
import PropTypes from 'prop-types';

import util from '../util';



class Command extends React.Component {
  constructor() {
    super();

    this.renderOutput = this.renderOutput.bind(this);
    this.hoverProcess = this.hoverProcess.bind(this);
  }

  hoverProcess(id) { this.props.hoverProcess(id); }

  renderOutput({ At, Text }) {
    const { instanceId } = this.props;
    const nodes = [];
    util.eachToken(Text, {
      onText: (text, i) => { nodes.push(<span key={i}>{text}</span>) },
      onPid: (pid, i) => {
        const path = `/instances/${instanceId}/process-info/${pid}`;
        nodes.push(<Link key={i} to={path}
          onMouseEnter={this.hoverProcess.bind(this, pid)} 
          onMouseLeave={this.hoverProcess.bind(this, null)}>

          {pid}
        </Link>);
      },
    });
    return <pre key={At}>{nodes}</pre>;
  }

  render() {
    return <div className="Command">
      <div className="input-container">
        <div className="prompt noselect">{this.props.cmd.Prompt}</div>
        <div className="input">{this.props.cmd.Input}</div>
      </div>
      <div className="outputs">
        {this.props.cmd.Outputs.map(this.renderOutput)}
      </div>
    </div>;
    return null;
  }
}
Command.propTypes = {
  cmd: PropTypes.object.isRequired,
  instanceId: PropTypes.string.isRequired,
  hoverProcess: PropTypes.func.isRequired,
}



@inject("store") @observer
export default class ShellPage extends React.Component {
  constructor() {
    super();

    this.onInputChange = this.onInputChange.bind(this);
    this.submitInput = this.submitInput.bind(this);
    this.hoverProcess = this.hoverProcess.bind(this);

    this.state = {
      text: ''
    }
  }

  componentDidMount() {
    this.props.store.ensureShellConnected();
  }

  hoverProcess(id) { this.props.store.hoverProcess(id); }

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
      result.push(<Command key={i}
        cmd={cmds[i]} instanceId={this.props.match.params.id}
        hoverProcess={this.hoverProcess} />);
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
