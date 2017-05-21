class App extends React.Component {
  constructor() {
    super();
    this.state = {
      errorText: null,
      tree: null
    };
  }

  componentDidMount() {
    let hash = window.location.hash;
    if (hash.slice(0,2) == '#/') {
      let url = '/prepared/' + hash.slice(2);
      this.fetchCsvAndLoad(url);
    } else {
      this.setState({errorText: 'Wrong '+hash+' hash address'});
    }
  }

  fetchCsvAndLoad(url) {
    fetch(url).then((response) => {
      response.text().then((text) => {
        let rows = CSV.parse(text);
        let keys = rows.shift();
        let tree = V.processEvents(keys, rows);
        this.setState({tree: tree});
      });
    });
  }

  render() {
    if (this.state.errorText) {
      return <div>{this.state.errorText}</div>;
    }

    if (this.state.tree) {
      return <svg style={{position: 'fixed', top: 0, left: 0, width:'100%', height: '100%'}}>
        <TreeView tree={this.state.tree} />
      </svg>;
    }

    return <div>Loading...</div>;
  }
}



document.addEventListener("DOMContentLoaded", function(event) {
  ReactDOM.render(<App />, document.getElementById('react-app'));
});