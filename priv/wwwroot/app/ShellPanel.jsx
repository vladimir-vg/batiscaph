class ShellPanel extends React.Component {
  constructor() {
    super();
    this.state = {
      viewportHeight: Math.max(document.documentElement.clientHeight, window.innerHeight || 0)
    }
  }

  renderDisabledShell() {
    return <div className="item disabled-underline">
      <div className="muted-text">no connection</div>
    </div>
  }

  render() {
    let maxHeight = this.state.viewportHeight - 40;

    let content = null;
    if (!this.props.shellInfo) {
      content = this.renderDisabledShell();
    }

    return <div style={{width: this.props.width, maxHeight: maxHeight}} className="ShellPanel">
      {content}
    </div>
  }
}