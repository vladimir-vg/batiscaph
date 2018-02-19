const Link = ReactRouterDOM.Link;

class MainPage extends React.Component {
  constructor() {
    super();
    this.state = {
      items: [],
      selectedIndex: null,
      nodeName: ''
    };

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.onBlockLeave = this.onBlockLeave.bind(this);
    this.editNodeName = this.editNodeName.bind(this);
    this.onConnectClick = this.onConnectClick.bind(this);
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

  editNodeName(e) {
    this.setState({nodeName: e.target.value});
  }

  onConnectClick() {
    this.props.connectToNode(this.state.nodeName);
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
    const path = '/scenarios/'+id;
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
        <div key={id} className={className} onMouseEnter={this.onLinkEnter.bind(this, i)}>
          <Link to={'/scenarios/'+id}>
            {id}
          </Link>
        </div>
      );
    }

    const connectButtonEnabled = this.state.nodeName && this.state.nodeName.indexOf("@") != -1;

    return <div id="MainPage" className="content-page">
      <div className="head-block">
        <p><button className="button" onClick={this.props.startNewShell}>Start new shell</button></p>
        <p>or connect to already running node:
        <input value={this.state.nodeName} onChange={this.editNodeName} />
         &nbsp; <button disabled={!connectButtonEnabled} onClick={this.onConnectClick} className="button">connect</button></p>
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
