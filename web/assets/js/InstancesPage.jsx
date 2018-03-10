import React from 'react';
import { observer, inject } from 'mobx-react';
import { Link } from 'react-router-dom';



// just to silence eslint, which cannot detect decorators usage
void(inject); void(observer);
@inject("store") @observer
export default class InstancesPage extends React.Component {
  componentWillMount() {
    this.props.store.fetchInstancesList();
  }

  renderLink({ InstanceId }) {
    return <div key={InstanceId}>
      <Link to={"/instances/" + InstanceId}>
        {InstanceId}
      </Link>
    </div>;
  }

  render() {
    const instances = this.props.store.instances.toJS().sort(({StartedAt: a}, {StartedAt: b}) => a < b ? 1 : -1);
    return <div>
      {instances.map(this.renderLink)}
    </div>;
  }
}
