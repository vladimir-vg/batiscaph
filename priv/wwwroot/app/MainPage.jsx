const Link = ReactRouterDOM.Link;

class MainPage extends React.Component {
  constructor() {
    super();
    this.state = {
      items: [],
      selectedIndex: null
    };

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.onBlockLeave = this.onBlockLeave.bind(this);
  }

  componentDidMount() {
    this.requestScenarousList();
  }

  requestScenarousList() {
    fetch('/scenarios.json').then((response) => {
      response.json().then((items) => {
        this.setState({items: items});
      });
    });
  }

  onLinkEnter(i) {
    this.setState({selectedIndex: i});
  }

  onBlockLeave() {
    this.setState({selectedIndex: null});
  }

  renderRecursiveTree(path, tree) {
    if (!tree) { return null; }

    let links = [];
    for (var key in tree) {
      const path1 = path + '/' + key;
      
      links.push(<li key={key}>
          <Link to={path + '/' + key}>{key}</Link>
          {this.renderRecursiveTree(path1, tree[key])}
        </li>
      );
    }
    return <ul>
      {links}
    </ul>;
  }

  renderHoveredContexts() {
    if (!this.state.selectedIndex) {
      return null;
    }
    const id = this.state.items[this.state.selectedIndex][0];
    const tree = this.state.items[this.state.selectedIndex][1];
    const path = '/scenarios2/'+id;
    return this.renderRecursiveTree(path, tree);
  }

  render() {
    let links = [];
    for (const i in this.state.items) {
      let id = this.state.items[i][0];
      let className = "scenario-item";
      if (i == this.state.selectedIndex) {
        className += " selected";
      }
      links.push(
        <div key={i}>
          <Link to={'/scenarios2/'+id} onMouseEnter={this.onLinkEnter.bind(this, i)} className={className}>
            {id}
          </Link>
        </div>
      );
    }

    return <div id="MainPage" className="content-page">
      <div className="head-block">
        <button className="btn" onClick={this.props.startNewShell}>Start new shell</button>
      </div>
      <br />
      <h1>Previous sessions:</h1>
      <div style={{display: 'flex'}} onMouseLeave={this.onBlockLeave}>
        <div style={{padding: 10}} className="scenarios-list-block">
          {links}
        </div>
        <div style={{flex: '1', padding: 10}}>
          {this.renderHoveredContexts()}
        </div>
      </div>
    </div>;
  }
}
