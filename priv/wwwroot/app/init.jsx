// pixels of empty space around figure that might be visible by dragging
V.WORKSPACE_PADDING = 100;



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
      let paddedWidth = Math.max(0, this.state.tree.maxX*(V.CELL_WIDTH + V.CELL_GUTTER) - V.CELL_GUTTER);
      let paddedHeight = this.state.tree.maxY*V.CELL_HEIGHT;
      return <SvgView padding={V.WORKSPACE_PADDING} paddedWidth={paddedWidth} paddedHeight={paddedHeight}>
        <TreeView tree={this.state.tree} />
      </SvgView>;
    }

    return <div>Loading...</div>;
  }
}



document.addEventListener("DOMContentLoaded", function(event) {
  ReactDOM.render(<App />, document.getElementById('react-app'));
});