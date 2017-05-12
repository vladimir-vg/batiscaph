class App extends React.Component {
  render() {
    return <div>Hello World! {100}</div>;
  }
};



document.addEventListener("DOMContentLoaded", function(event) {
  ReactDOM.render(<App />, document.getElementById('react-app'));
});