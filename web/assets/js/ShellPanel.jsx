import React from 'react';
import PropTypes from 'prop-types';



export default class ShellPanel extends React.Component {
  componentDidMount() {
    this.props.store.ensureShellConnected();
  }

  render() {
    return <div />;
  }
}
ShellPanel.propTypes = {
  store: PropTypes.object.isRequired,
}