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

  tryRenderPid(pid) {
    if (!pid) return null;
    if (pid[0] != '<') return null;
    return this.renderPid(pid);
  }

  render() {
    if (!this.props.selectedItem) {
      return null;
    }

    let body = null;

    if (this.props.selectedItem.type == 'proc') {
      let proc = this.props.tree.procs[this.props.selectedItem.key];
      body = <div>
        <div>pid: {this.tryRenderPid(proc.pid)}</div>
        <div>mfa: {proc.mfa}</div>
        <div>parent: {this.tryRenderPid(proc.parent)}</div>
        <pre>{this.renderTextContent(proc.reason)}</pre>
      </div>;
    } else if (this.props.selectedItem.type == 'send') {
      let send = this.props.tree.sends[this.props.selectedItem.key];
      body = <div>
        <div>from: {this.tryRenderPid(send.from)}</div>
        <div>to: {this.tryRenderPid(send.to)}</div>

        <pre>{this.renderTextContent(send.term)}</pre>
      </div>;
    }

    // width is hardcoded, 450 - 20 - 20 ()
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