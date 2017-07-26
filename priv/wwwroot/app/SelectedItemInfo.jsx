class SelectedItemInfo extends React.Component {
  onItemSelect(type, key) {
    this.props.onItemSelect({type: type, key: key});
  }

  onCellHoverEnter(type, key) {
    this.props.onItemHover({type: type, key: key});
  }
  onCellHoverLeave() {
    this.props.onItemHover(null);
  }

  renderPid(pid, key) {
    key = key || pid;
    let className = "pid";
    if (this.props.hoveredItem && this.props.hoveredItem.type == 'proc' && this.props.hoveredItem.key == pid) {
      className += " hovered";
    }

    if (this.props.selectedItem && this.props.selectedItem.type == 'proc' && this.props.selectedItem.key == pid) {
      className += " selected";
    }

    return <span key={key} className={className}
      onClick={this.onItemSelect.bind(this, 'proc', pid)}
      onMouseEnter={this.onCellHoverEnter.bind(this, 'proc', pid)}
      onMouseLeave={this.onCellHoverLeave.bind(this)}>{pid}</span>
  }

  renderTextContent(text) {
    if (!text) return "";

    let parts = [];
    let onText = (text, pos) => {
      parts.push(<span key={pos}>{text}</span>);
    }

    let onPid = (pid, pos) => {
      parts.push(this.renderPid(pid, pos));
    }

    V.walkInfoText(text, onText, onPid);

    return parts;
  }

  tryRenderPid(pid, key) {
    if (!pid) return null;
    if (pid[0] != '<') return pid;
    return this.renderPid(pid, key);
  }

  renderProc() {
    let proc = this.props.tree.procs[this.props.selectedItem.key];
    let registered = null;
    let terminatedAttr = null;
    let terminated = null;
    if (proc.atom) {
      registered = <div>registered as: <code>{proc.atom}</code></div>;
    }
    if (proc.reason) {
      terminatedAttr = <div>
        <br />
        <div>termination reason:</div>
      </div>
      terminated = <pre>{this.renderTextContent(proc.reason)}</pre>;
    }
    return <div>
      <div className="attrs">
        <div>process: {this.tryRenderPid(proc.pid)}</div>
        <div>parent: {this.tryRenderPid(proc.parent)}</div>
        <div>spawn as: <code>{proc.mfa}</code></div>
        {registered}
        {terminatedAttr}
      </div>
      {terminated}
    </div>;
  }

  renderMessageSend() {
    let send = this.props.tree.sends[this.props.selectedItem.key];
    let receiver = null;

    if (send.to && send.toAtom) {
      receiver = <span>{this.tryRenderPid(send.to, 'to')} (<code>{send.toAtom}</code>)</span>;
    } else if (send.to) {
      receiver = <span>{this.tryRenderPid(send.to, 'to')}</span>;
    } else {
      receiver = <code>{send.toAtom}</code>;
    }

    return <div>
      <div className="attrs">message {this.tryRenderPid(send.from, 'from')} &rarr; {receiver}</div>
      <pre>{this.renderTextContent(send.term)}</pre>
    </div>;
  }

  render() {
    if (!this.props.selectedItem) {
      return null;
    }

    let body = null;
    if (this.props.selectedItem.type == 'proc') {
      body = this.renderProc();
    } else if (this.props.selectedItem.type == 'send') {
      body = this.renderMessageSend();
    }

    return <div className="selected-item-info">
      {body}
    </div>;
  }
};

SelectedItemInfo.propTypes = {
  tree: React.PropTypes.object.isRequired,
  onItemSelect: React.PropTypes.func.isRequired,
  onItemHover: React.PropTypes.func.isRequired,
  selectedItem: React.PropTypes.object,
  hoveredItem: React.PropTypes.object,
};