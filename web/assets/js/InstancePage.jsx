import React from 'react';
import { observer, inject } from 'mobx-react';
void(inject); void(observer); // just to silence eslint, which cannot detect decorators usage

import HttpReq from './elements/HttpReq';



const hGap = 4;
const colWidth = 10;
const rowHeight = 3;
const grid = {
  xColStart: (x) => { return x*(colWidth+hGap) + hGap },
  xColWidth: colWidth,
  yRowAt: (y) => { return y*rowHeight },
  yRowHeight: rowHeight,
};



@inject("store") @observer
export default class InstancePage extends React.Component {
  componentWillMount() {
    this.props.store.subscribeToInstance(this.props.match.params.id);
  }

  componentWillUnmount() {
    this.props.store.unsubscribeFromInstance(this.props.match.params.id);
  }

  renderGrid() {
    const cols = [];
    const rows = [];
    for (let i = 0; i < 100; i++) {
      cols.push(<rect key={'col'+i}
        style={{fill: 'rgba(255,0,0,0.08)'}}
        x={grid.xColStart(i)} y={grid.yRowAt(0)}
        width={grid.xColWidth} height={1000} />);
    }
    for (let i = 0; i < 100; i += 2) {
      rows.push(<rect key={'row'+i}
        style={{fill: 'rgba(255,0,0,0.08)'}}
        x={0} y={grid.yRowAt(i)} width={1000} height={grid.yRowHeight} />);
    }

    return <g>{cols}{rows}</g>;
  }

  renderElement(C, e) {
    const { id, x1, x2, y1, y2, attrs } = e;
    const props = { id, x1, x2, y1, y2, attrs };
    return <C key={id} grid={grid} {...props} />;
  }

  render() {
    const reqs = this.props.store.layout.HttpReq || [];
    // console.log(reqs, mobx.toJS(reqs));
    return <div>
      <svg width={300} height={300} style={{border: '1px rgba(255,0,0,0.1) solid'}}>
        {this.renderGrid()}
        {reqs.map(this.renderElement.bind(this, HttpReq.Component))}
      </svg>
    </div>;
  }
}
