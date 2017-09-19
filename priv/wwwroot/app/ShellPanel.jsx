let shortPrompt = function (prompt) {
  return /(\d+>)/.exec(prompt)[0];
}



class ShellPanel extends React.Component {
  constructor() {
    super();
    this.state = {
      viewportHeight: Math.max(document.documentElement.clientHeight, window.innerHeight || 0)
    }
  }q

  renderInputItem(e) {
    return <div key={e.at} className="item input">
      <code className="prompt unselectable">{shortPrompt(e.prompt)}</code>
      <code className="message">{e.message}</code>
    </div>;
  }

  renderOutputItem(e) {
    return <div key={e.at} className="item output">
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
        <div className="item active underline">
          <code className="prompt unselectable">{shortPrompt(this.props.prompt)}</code>
          <textarea>please type here</textarea>
        </div>
      </div>;
    }
  }

  render() {
    let maxHeight = this.state.viewportHeight - 40;

    return <div style={{width: this.props.width, maxHeight: maxHeight}} className="ShellPanel">
      {this.renderEvents()}
      {this.renderPrompt()}
    </div>
  }
}