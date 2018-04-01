import React from 'react';
import PropTypes from 'prop-types';

import c from '../constraint';



class Component extends React.Component {
  render() {
    return <g className="Process" />;
  }
}
Component.propTypes = {
}



function produceElements(delta) {
  const result = [];

  for (const pid in delta['erlang-processes']) {
    const proc = delta['erlang-processes'][pid];
    const constraints = {
      x: c.xPid(proc.Pid),
      startedY: c.yTimestamp(proc.SpawnedAt || proc.TraceStartedAt),
    }
    if (proc.SpawnedAt) {
      constraints.parentX = c.xPid(proc.ParentPid);
    }

    if (proc.ExitedAt) {
      constraints.exitedY = c.yTimestamp(proc.ExitedAt);
    } else {
      constraints.continueY = c.yTimestampNow();
    }

    result.push({ id: pid, key: pid, Component, constraints });
  }
  return result;
};


export default {
  produceElements // , Component
};