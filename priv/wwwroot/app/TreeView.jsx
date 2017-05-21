class TreeView extends React.Component {
  componentDidMount() {
    console.log(this.props.tree)
  }

  render() {
    return <div>Loading tree view...</div>;
  }
};

TreeView.propTypes = {
  tree: React.PropTypes.object.isRequired
};