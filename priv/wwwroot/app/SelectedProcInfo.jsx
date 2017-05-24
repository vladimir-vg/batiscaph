class SelectedProcInfo extends React.Component {
  render() {
    if (!this.props.pid) {
      return <div />;
    }

    let proc = this.props.tree.procs[this.props.pid];
    return <div style={{position: 'fixed', right: 100, top: 100}}>
      <div>pid: {this.props.pid}</div>
      <div>mfa: {proc.mfa}</div>
      <div>parent: {proc.parent}</div>
      <pre>{proc.reason}</pre>
    </div>;
  }
};

SelectedProcInfo.propTypes = {
  tree: React.PropTypes.object.isRequired,
  pid: React.PropTypes.string,
};