let Route = ReactRouterDOM.Route;

class ScenarioView2 extends React.Component {
  componentDidMount() {
    this.state = {
      tree: undefined
    };

    this._layout = {};
    let url = '/api/scenarios2/' + this.props.match.params.id;
    this.fetchInitialTree(url);
  }

  fetchInitialTree(url) {
    fetch(url).then((response) => {
      response.json().then((delta) => {
        V.updateLayout(delta, this._layout);
        let tree = V.produceTree(this._layout);
        this.setState({tree: tree});
      });
    });
  }

  render() {
    return <svg>
    </svg>;
  }
}

class App2 extends React.Component {
  constructor() {
    super();
    this.state = {
    };

    // avoid repeating .bind(this) by doing it once
  }

  render() {
    return <div>
      <Route path="/scenarios2/:id" component={ScenarioView2} />
    </div>;
  }
};