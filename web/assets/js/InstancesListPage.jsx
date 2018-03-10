import React from 'react';
import { observer, inject } from 'mobx-react';
import { Link } from 'react-router-dom';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage



@inject("store") @observer
export default class InstancesListPage extends React.Component {
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
    const instances = this.props.store.instancesList.toJS().sort(({StartedAt: a}, {StartedAt: b}) => a < b ? 1 : -1);
    return <div>
      {instances.map(this.renderLink)}
    </div>;
  }
}
