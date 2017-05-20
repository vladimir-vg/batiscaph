class App extends React.Component {
  constructor() {
    super();
    this.state = {
      errorText: null,
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
        console.log("parsed rows: ", rows);
      });
    });
  }

  render() {
    return <div>Hello World! {100}</div>;
  }
};



document.addEventListener("DOMContentLoaded", function(event) {
  ReactDOM.render(<App />, document.getElementById('react-app'));
});