import React from 'react';
import ReactDOM from 'react-dom';

const { Provider, Consumer } = React.createContext();



class WithLayout extends React.Component {
  constructor() {
    super();

    this.renderWithRefs = this.renderWithRefs.bind(this);
  }

  renderWithRefs(refs) {
    if (!(this.props.name in refs)) {
      throw {
        message: "Unexpected layout key",
        name: this.props.name
      };
    }

    // layout node is not mounted yet
    if (!refs[this.props.name].current) {
      return null;
    }

    return ReactDOM.createPortal(this.props.children, refs[this.props.name].current);
  }

  render() {
    return <Consumer>
      {this.renderWithRefs}
    </Consumer>
  }
}

export default {
  Provider, WithLayout
};