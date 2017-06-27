class ScenariosList extends React.Component {

  // currently router is not used yet
  // force reload after location hash change
  // quick and dirty
  forceReload() {
    window.location.reload();
  }

  renderScenarioBlock(block) {
    let items = [];
    for (let i in block.items) {
      let path = "/#/" + block.dir + "/" + block.items[i];
      items.push(<div key={block.items[i]}>
        <a href={path} onClick={this.forceReload.bind(this)} className="item">{block.items[i]}</a>
      </div>);
    }

    return <div key={block.dir} className="scenarios-list-block">
      <h1>{block.dir}</h1>
      <div>{items}</div>
    </div>;
  }

  render() {
    let blocks = [];
    for (let i in this.props.scenarios) {
      blocks.push(this.renderScenarioBlock(this.props.scenarios[i]))
    }

    return <div>
      {blocks}
    </div>;
  }
};



ScenariosList.propTypes = {
  scenarios: React.PropTypes.array.isRequired
};