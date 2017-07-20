class ScenariosList extends React.Component {

  // currently router is not used yet
  // force reload after location hash change
  // quick and dirty
  forceReload() {
    window.location.reload();
  }

  renderScenario(scenario) {
    let path = "/#/" + scenario.instance_id;
    return <div key={scenario.instance_id}>
      <a href={path} onClick={this.forceReload.bind(this)} className="item">{scenario.instance_id}</a>
    </div>;
  }

  render() {
    let scenarios = [];
    for (let i in this.props.scenarios) {
      scenarios.push(this.renderScenario(this.props.scenarios[i]))
    }

    return <div className="scenarios-list-block">
      {scenarios}
    </div>;
  }
};



ScenariosList.propTypes = {
  scenarios: React.PropTypes.array.isRequired
};