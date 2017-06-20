class SelectedItemInfo extends React.Component {
  render() {
    if (!this.props.selectedItem) {
      return null;
    }

    let body = null;

    if (this.props.selectedItem.type == 'proc') {
      let proc = this.props.tree.procs[this.props.selectedItem.key];
      body = <div>
        <div>pid: {proc.pid}</div>
        <div>mfa: {proc.mfa}</div>
        <div>parent: {proc.parent}</div>
        <pre>{proc.reason}</pre>
      </div>;
    } else if (this.props.selectedItem.type == 'send') {
      let send = this.props.tree.sends[this.props.selectedItem.key];
      body = <div>
        <div>from: {send.from}</div>
        <div>to: {send.to}</div>

        <pre>{send.term}</pre>
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
  selectedItem: React.PropTypes.object,
  hoveredItem: React.PropTypes.object,
};