let shortPrompt = function (prompt) {
  return /(\d+>)/.exec(prompt)[0];
}



const trimLastNewline = (str) => {
  if (str[str.length-1] == "\n") {
    return str.slice(0,str.length-1);
  }
  return str;
};



class SourcePanel extends React.Component {
  constructor() {
    super();
    this.state = {
      viewportHeight: Math.max(document.documentElement.clientHeight, window.innerHeight || 0),
      text: "",

      // currently selected line from history
      // indicates just index from props.events array
      historyIndex: null,
      lineHover: null // mouse currently over this line
    }

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.onKeyPress = this.onKeyPress.bind(this);
    this.onKeyDown = this.onKeyDown.bind(this);
    this.onTextChange = this.onTextChange.bind(this);
  }

  onTextChange(e) {
    let text = e.target.value;
    if (this.state.text === '' && text === "\n") {
      // do not make newline when input is empty
      // also it solves problem of race of events (keyPress and change)
      // after hitting Enter
      return;
    }
    // change of the text immediately resets current position
    // in history selection
    this.setState({text: text, historyIndex: null});
  }

  onKeyPress(e) {
    // plain Enter makes new line
    // Shift + Enter sends input to remote shell
    if (e.key === 'Enter' && !e.shiftKey && this.state.text) {
      console.log("---\nshell input: ", this.state.text);
      this.props.submitInput(this.state.text);
      this.setState({text: ""});
    }
  }

  onKeyDown(e) {
    const canNavigateInHistory = (this.state.text === '') || this.state.historyIndex;

    if (e.key === "ArrowUp" && canNavigateInHistory) {
      this.moveHistoryBack();
    } else if (e.key === "ArrowDown" && canNavigateInHistory) {
      this.moveHistoryForward();
    }
  }

  moveHistoryBack() {
    // if index is not present, we're not in selecting history mode
    // then just take last shell input in events

    let index = this.state.historyIndex;
    if (index === null) {
      index = this.props.events.length;
    }

    for (let i = index-1; i--; i >= 0) {
      const e = this.props.events[i];
      if (e.type == 'shell_input') {
        // okay, found a closest one
        // set text to it, keep index in case
        // if user wants to continue to move in history
        this.setState({historyIndex: i, text: trimLastNewline(e.message)});
        return;
      }
    }

    // if we ran out of available history, then do nothing
  }

  moveHistoryForward() {
    // move forward only makes sense if we already moved backward
    if (this.state.historyIndex === null) { return; }

    for (let i = this.state.historyIndex+1; i < this.props.events.length; i++) {
      const e = this.props.events[i];
      if (e.type == 'shell_input') {
        // okay, found a closest one
        // set text to it, keep index in case
        // if user wants to continue to move in history
        this.setState({historyIndex: i, text: trimLastNewline(e.message)});
        return;
      }
    }

    // if we ran out of available history, then do nothing
  }

  setLineHover(line) {
    let context = this.props.contexts[this.props.selectedContext];
    if (context.evals[line]) {
      this.setState({lineHover: line})
      this.props.highlightRange(context.evals[line]);
    } else {
      this.setState({lineHover: null})
      this.props.highlightRange(null);
    }
  }

  renderInputItem(e) {
    return <div key={e.at} className="item input top-padded">
      <code className="prompt unselectable">{shortPrompt(e.prompt)}</code>
      <code className="message">{e.message}</code>
    </div>;
  }

  renderOutputItem(e) {
    return <div key={e.at} className="item muted">
      <code className="prompt unselectable"></code>
      <code className="message">{e.message}</code>
    </div>;
  }

  renderEvents() {
    let items = [];
    for (var i in this.props.events) {
      let e = this.props.events[i];
      if (e.type == 'shell_input') {
        items.push(this.renderInputItem(e));
      } else if (e.type == 'shell_output') {
        items.push(this.renderOutputItem(e));
      }
    }
    return items;
  }

  renderPrompt() {
    if (this.props.prompt) {
      return <div>
        <div className="item active underline top-padded">
          <code className="prompt unselectable">{shortPrompt(this.props.prompt)}</code>
          <textarea value={this.state.text} rows={3} ref={(ref) => { this.textareaRef = ref; }}
            autoFocus onChange={this.onTextChange} onKeyPress={this.onKeyPress} onKeyDown={this.onKeyDown} />
        </div>
      </div>;
    }
  }

  renderLines(context) {
    let nodes = [];
    for (var i in context.lines) {
      let line = context.lines[i];
      let className = "item source";
      if (line[0] == this.state.lineHover) {
        className += " highlighted";
      }
      nodes.push(
        <div onMouseEnter={this.setLineHover.bind(this, line[0])} onMouseLeave={this.setLineHover.bind(this, null)}
            key={line[0]} className={className}>

          <code className="prompt unselectable">{line[0]}</code>
          <code className="message">{line[1]}</code>
        </div>
      );
    }
    return nodes;
  }

  renderSend(send) {
    return <div>
      <div className="item muted">
        <code className="prompt unselectable">&nbsp;</code>
        <code className="message unselectable">{send.fromPid} &rarr; {send.toPid}</code>
      </div>
      <div className="item top-padded">
        <code className="prompt unselectable">&nbsp;</code>
        <code className="message">{send.term}</code>
      </div>
    </div>;
  }

  render() {
    let maxHeight = this.state.viewportHeight - (40 + 60);

    if (this.props.selectedContext) {
      const context = this.props.contexts[this.props.selectedContext];
      if (context.lines) {
        return <div style={{width: this.props.width, maxHeight: maxHeight}} className="SourcePanel">
        {this.renderLines(context)}
        </div>;
      }
      // if context doesn't have source lines
      // just display shell as usual
    }

    if (this.props.selectedSend) {
      const send = this.props.sends[this.props.selectedSend];
      return <div style={{width: this.props.width, maxHeight: maxHeight}} className="SourcePanel">
      {this.renderSend(send)}
      </div>;
    }

    return <div style={{width: this.props.width, maxHeight: maxHeight}} className="SourcePanel">
      {this.renderEvents()}
      {this.renderPrompt()}
    </div>;
  }
}
SourcePanel.propTypes = {
  contexts: PropTypes.object.isRequired,
  highlightRange: PropTypes.func.isRequired,
  selectedContext: PropTypes.string
};