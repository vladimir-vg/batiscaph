const Link = ReactRouterDOM.Link;

class MainPage extends React.Component {
  constructor() {
    super();
    this.state = {
      items: []
    };
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

  render() {
    let links = [];
    for (const i in this.state.items) {
      let id = this.state.items[i].instance_id;
      links.push(<div key={i}><Link to={'/scenarios2/'+id} className="item">{id}</Link></div>);
    }

    return <div className="content-page">
      <div className="head-block">
        <button className="btn" onClick={this.props.startNewShell}>Start new shell</button>
      </div>
      <br />
      <div className="scenarios-list-block">
        {links}
      </div>
    </div>;
  }
}
