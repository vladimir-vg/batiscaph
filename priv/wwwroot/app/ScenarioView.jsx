const Link = ReactRouterDOM.Link;


const CELL_WIDTH = 10;
const CELL_HEIGHT = 10;
const CELL_HGUTTER = 10;
const PORT_WIDTH = 6;
const PORT_BODY_SHIFT = 2; // how many pixels of port body should be outside process
const MENTION_PADDING = 2;
const SOURCE_PANEL_WIDTH = 520;

const LABEL_XPADDING = 4;
const LABEL_YPADDING = 4;



class Layer extends React.Component {
  constructor() {
    super();
    this.state = {
      node: null
    }
  }

  componentDidMount() {
    this.setState({node: this.context.getLayerNode(this.props.name)});
  }

  render() {
    if (!this.state.node) {
      return null;
    }

    // let node = this.context.getLayerNode(this.props.name);
    return ReactDOM.createPortal(this.props.children, this.state.node);
  }
}
Layer.contextTypes = {
  getLayerNode: PropTypes.func
}
Layer.propTypes = {
  name: PropTypes.string
};



class ProcessElement extends React.Component {
  constructor() {
    super();
    this.state = {
      displayText: false
    }

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.onMouseEnter = this.onMouseEnter.bind(this);
    this.onMouseLeave = this.onMouseLeave.bind(this);
    this.onClick = this.onClick.bind(this);
  }

  onMouseEnter() {
    this.setState({displayText: true});
  }

  onMouseLeave() {
    this.setState({displayText: false})
  }

  onClick() {
    this.props.tracePid(this.props.data.pid);
  }

  renderTracedPart(part, i) {
    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x;
    let y = CELL_HEIGHT*part.fromY;
    let width = CELL_WIDTH;
    let height = CELL_HEIGHT*(part.toY - part.fromY);

    let untracedNode = null;
    if (part.untracedHead) {
      let mX = x + MENTION_PADDING;
      let mWidth = width - 2*MENTION_PADDING;
      untracedNode = <g>
        <rect x={x} y={y-CELL_HEIGHT/2} width={width} height={CELL_HEIGHT/2} style={{fill: '#D9D9D9'}} />
        <rect x={mX} y={y-CELL_HEIGHT/2} width={mWidth} height={CELL_HEIGHT/2} style={{fill: '#EDEDED'}} />
      </g>;
    }

    let exitRect = null;
    if (part.exitMark) {
      let h = part.unnormalExit ? (CELL_HEIGHT/2) : 2;
      exitRect = <rect x={x} y={y+height-h} width={width} height={h} className="exit-reason" />
    }
    return <g key={i}>
      {untracedNode}
      <rect x={x} y={y} width={width} height={height} style={{fill: '#D9D9D9'}} />
      {exitRect}
    </g>;
  }

  renderMentionPart(part, i) {
    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x;
    let y = CELL_HEIGHT*part.y;
    let y1 = y - Math.floor(CELL_HEIGHT/2);
    let width = CELL_WIDTH;
    let height = CELL_HEIGHT;

    let mX = x + MENTION_PADDING;
    let mWidth = width - 2*MENTION_PADDING;

    // render only first and last mention in process
    // all others supposed to be marked with small grey dots
    if (i == 0 || i == this.props.data.parts.length-1) {
      return <g key={i}>
        <rect x={x} y={y1} width={width} height={height} style={{fill: '#D9D9D9'}} />
        <rect x={mX} y={y1} width={mWidth} height={height} style={{fill: '#EDEDED'}} />
      </g>
    }

    return <circle key={i} cx={x+CELL_WIDTH/2} cy={y} r={2} style={{fill: '#D9D9D9'}} />;
  }

  renderDeadPart(part, i) {
    let crossSize = Math.min(CELL_WIDTH, CELL_HEIGHT);

    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x;
    let y = CELL_HEIGHT*part.y - CELL_HEIGHT/2;

    let x1 = x+(CELL_WIDTH-crossSize)/2;
    let y1 = y+(CELL_HEIGHT-crossSize)/2;
    let x2 = x+CELL_WIDTH-(CELL_WIDTH-crossSize)/2;
    let y2 = y+CELL_HEIGHT-(CELL_HEIGHT-crossSize)/2;

    return <g key={i}>
      <line x1={x1} y1={y1} x2={x2} y2={y2} style={{stroke: '#FC8888', strokeWidth: 2}} />
      <line x1={x2} y1={y1} x2={x1} y2={y2} style={{stroke: '#FC8888', strokeWidth: 2}} />
    </g>;
  }

  renderConnectingLine() {
    if (this.props.data.parts.length < 2) return null;
    let startY = this.props.data.startY;
    let lastPart = this.props.data.parts[this.props.data.parts.length-1];
    let stopY = lastPart.y || lastPart.toY;

    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x + CELL_WIDTH/2;
    let y1 = CELL_HEIGHT*startY - CELL_HEIGHT/2;
    let y2 = CELL_HEIGHT*stopY - CELL_HEIGHT/2;

    return <line x1={x} y1={y1} x2={x} y2={y2} className="process-connecting-line" />;
  }

  renderTextInfo() {
    let startY = this.props.data.startY;
    let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x;
    let y = CELL_HEIGHT*startY - CELL_HEIGHT;
    let text = "";
    // if (this.props.data.registeredName) {
    //   text = "."; // show that we have something to display
    // }
    if (this.state.displayText) {
      text = this.props.data.registeredName || this.props.data.pid;
    }

    return <text x={x} y={y} className="label">{text}</text>;
  }

  render() {
    let nodes = [];
    for (const i in this.props.data.parts) {
      let part = this.props.data.parts[i];
      if (part.type == 'TRACED') {
        nodes.push(this.renderTracedPart(part, i));
      } else if (part.type == 'MENTION') {
        nodes.push(this.renderMentionPart(part, i));
      } else if (part.type == 'DEAD') {
        nodes.push(this.renderDeadPart(part, i));
      }
    }
    return [
      <Layer key="processes" name="processes">
        <g onMouseEnter={this.onMouseEnter} onMouseLeave={this.onMouseLeave} onClick={this.onClick}>
          {this.renderConnectingLine()}
          {nodes}
        </g>
      </Layer>,
      <Layer key="text" name="text">
        {this.renderTextInfo()}
      </Layer>
    ];
  }
}



class SpawnElement extends React.Component {
  render() {
    let width = 2;
    let fromX = this.props.data.fromX, toX = this.props.data.toX;
    if (fromX < toX) {
      let x1 = (CELL_WIDTH+CELL_HGUTTER)*fromX + CELL_WIDTH/2;
      let y = CELL_HEIGHT*this.props.data.y;
      let x2 = (CELL_WIDTH+CELL_HGUTTER)*toX + CELL_WIDTH;
      return [
        <Layer key="spawnsBehindProcesses" name="spawnsBehindProcesses">
          <line x1={x1} y1={y} x2={x2} y2={y} className="spawn-line" />
        </Layer>,
        <Layer key="spawnsOnProcesses" name="spawnsOnProcesses">
          {/* |_*/}
          <line x1={x1} y1={y+width/2} x2={x1} y2={y-CELL_HEIGHT/2} className="spawn-line" />
          <line x1={x1} y1={y} x2={x1+CELL_WIDTH/2+PORT_BODY_SHIFT} y2={y} className="spawn-line" />

          <line x1={x2-width/2} y1={y} x2={x2-width/2} y2={y+CELL_HEIGHT/2} className="spawn-line" />
          <line x1={x2-CELL_WIDTH} y1={y} x2={x2} y2={y} className="spawn-line" />
        </Layer>
      ];
    } else {
      let x1 = (CELL_WIDTH+CELL_HGUTTER)*toX;
      let y = CELL_HEIGHT*this.props.data.y;
      let x2 = (CELL_WIDTH+CELL_HGUTTER)*fromX+CELL_WIDTH/2;
      return [
        <Layer key="spawnsBehindProcesses" name="spawnsBehindProcesses">
          <line x1={x1} y1={y} x2={x2} y2={y} className="spawn-line" />
        </Layer>,
        <Layer key="spawnsOnProcesses" name="spawnsOnProcesses">
          <line x1={x2} y1={y+width/2} x2={x2} y2={y-CELL_HEIGHT/2} className="spawn-line" />
          <line x1={x2} y1={y} x2={x2-CELL_WIDTH/2} y2={y} className="spawn-line" />

          <line x1={x1+width/2} y1={y} x2={x1+width/2} y2={y+CELL_HEIGHT/2+width/2} className="spawn-line" />
          <line x1={x1} y1={y} x2={x1+CELL_WIDTH} y2={y} className="spawn-line" />
        </Layer>
      ];
    }
  }
}



class LinkElement extends React.Component {
  render() {
    let xs = [this.props.data.fromX, this.props.data.toX];
    xs.sort((a,b) => a-b);
    let x1 = (CELL_WIDTH+CELL_HGUTTER)*xs[0];
    let y = CELL_HEIGHT*this.props.data.y;
    let x2 = (CELL_WIDTH+CELL_HGUTTER)*xs[1] + CELL_WIDTH;
    return [
      <Layer key="linksBehindProcesses" name="linksBehindProcesses">
        <line x1={x1} y1={y} x2={x2} y2={y} className="link-line" />
      </Layer>,
      <Layer key="linksOnProcesses" name="linksOnProcesses">
        <line x1={x1} y1={y} x2={x1+CELL_WIDTH+PORT_BODY_SHIFT} y2={y} className="link-line" />
        <line x1={x2} y1={y} x2={x2-CELL_WIDTH} y2={y} className="link-line" />
      </Layer>
    ];
  }
}



class MentionElement extends React.Component {
  render() {
    let xs = [this.props.data.fromX, this.props.data.toX];
    xs.sort((a,b) => a-b);
    let x1 = (CELL_WIDTH+CELL_HGUTTER)*xs[0];
    let y = CELL_HEIGHT*this.props.data.y;
    let x2 = (CELL_WIDTH+CELL_HGUTTER)*xs[1] + CELL_WIDTH;

    return <Layer name="mentions">
      <line x1={x1+CELL_WIDTH/2} y1={y} x2={x2} y2={y} style={{stroke: '#EDEDED', strokeWidth: 2}} />
    </Layer>;
  }
}



class PointElement extends React.Component {
  render() {
    let width = CELL_WIDTH+4;
    let height = 2;

    let x = Math.floor((CELL_WIDTH+CELL_HGUTTER)*this.props.data.x + (CELL_WIDTH - width)/2);
    let y = Math.floor(CELL_HEIGHT*this.props.data.y - height/2);
    return <Layer name="points">
      <rect x={x} y={y} width={width} height={height} style={{fill: '#BDBDBD'}} />
    </Layer>;
  }
}



const WPADDING = 0; // CELL_HGUTTER/2;
const HPADDING = CELL_HEIGHT/2;

class ContextElement extends React.Component {
  constructor() {
    super();
    this.state = {
      mentionHover: null // key of the mention which is hovered
    }

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.onSelectClick = this.onSelectClick.bind(this);
    this.onMouseLeaveMention = this.onMouseLeaveMention.bind(this);
  }

  onSelectClick() {
    if (this.props.selectedContext == this.props.data.key) {
      this.props.selectContext(null);
    } else {
      this.props.selectContext(this.props.data.key);
    }
  }

  onMouseEnterMention(key) {
    this.setState({mentionHover: key});
  }

  onMouseLeaveMention() {
    this.setState({mentionHover: null})
  }

  renderMentions(mentions, className) {
    let lines = [];
    let selectables = [];
    const selectableHeight = 8;
    for (var key in mentions) {
      let m = mentions[key];
      let y = CELL_HEIGHT*m.y;
      let x1, x2;
      if (this.props.data.x < m.toX) {
        x1 = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x + CELL_WIDTH + WPADDING;
        x2 = (CELL_WIDTH+CELL_HGUTTER)*m.toX;
      } else {
        x1 = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x - WPADDING;
        x2 = (CELL_WIDTH+CELL_HGUTTER)*m.toX + CELL_WIDTH;
      }

      // actualy visible node
      lines.push(
        <line key={key} x1={x1} y1={y} x2={x2} y2={y} className={className} />
      );

      // clickable area
      selectables.push(
        <rect key={key + "-selection"} x={Math.min(x1,x2)} y={y - selectableHeight/2} width={Math.abs(x2-x1)} height={selectableHeight}
          onClick={this.onSelectClick}
          style={{fill: 'transparent'}}
          onMouseEnter={this.onMouseEnterMention.bind(this, key)}
          onMouseLeave={this.onMouseLeaveMention} />
      );
    }
    return {lines, selectables};
  }

  renderHoveredMentionText() {
    if (!this.state.mentionHover) { return null; }
    let m = this.props.data.mentions[this.state.mentionHover];
    let y = CELL_HEIGHT*m.y;
    if (this.props.data.x < m.toX) {
      let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x + CELL_WIDTH + WPADDING;
      return <text x={x+LABEL_XPADDING} y={y-LABEL_YPADDING} className="label">{m.expr}</text>;
      // x2 = (CELL_WIDTH+CELL_HGUTTER)*m.toX;
    } else {
      let x = (CELL_WIDTH+CELL_HGUTTER)*this.props.data.x - WPADDING;
      return <text x={x-LABEL_XPADDING} y={y-LABEL_YPADDING} textAnchor="end" className="label">{m.expr}</text>;
      // x2 = (CELL_WIDTH+CELL_HGUTTER)*m.toX + CELL_WIDTH;
    }
  }

  renderContextText() {
    const parts = this.props.data.key.split(' ');
    const text = parts[parts.length-1];

    let x = 1 + this.props.data.x*(CELL_WIDTH+CELL_HGUTTER) - WPADDING;
    let y = this.props.data.fromY*CELL_HEIGHT - HPADDING;
    return <text x={x} y={y-LABEL_YPADDING} className="label">{text}</text>;
  }

  render() {
    let ys = [this.props.data.fromY, this.props.data.toY];
    ys.sort((a,b) => a-b);

    let radius = Math.floor(Math.min(WPADDING, HPADDING));
    let x = 1 + this.props.data.x*(CELL_WIDTH+CELL_HGUTTER) - WPADDING;
    let y = this.props.data.fromY*CELL_HEIGHT - HPADDING;
    let width = -2 + CELL_WIDTH + WPADDING*2;
    let height = (this.props.data.toY - this.props.data.fromY)*CELL_HEIGHT + HPADDING*2;

    if (this.props.selectedContext == this.props.data.key) {
      const contextText = this.renderContextText();
      let mentionText = this.renderHoveredMentionText();
      const {lines, selectables} = this.renderMentions(this.props.data.mentions, "var-mention active");
      return [
        <Layer key="inactiveContexts" name="inactiveContexts"></Layer>,
        <Layer key="activeContexts" name="activeContexts">
          <rect onClick={this.onSelectClick} className="context active"
            x={x} y={y} width={width} height={height} />
          <g>{lines}</g>
        </Layer>,
        <Layer key="text" name="text">
          {mentionText}
          {contextText}
        </Layer>,
        <Layer key="selection" name="selection">
          {selectables}
        </Layer>
      ];
    }

    const {lines, selectables} = this.renderMentions(this.props.data.mentions, "var-mention");
    return [
      <Layer key="inactiveContexts" name="inactiveContexts">
        <rect onClick={this.onSelectClick} className="context"
          x={x} y={y} width={width} height={height} />
      </Layer>,
      <Layer key="inactiveContextsLines" name="inactiveContextsLines">
        <g>{lines}</g>
      </Layer>,
      <Layer key="activeContexts" name="activeContexts"></Layer>,
      <Layer key="text" name="text"></Layer>,
      <Layer key="selection" name="selection">
        {selectables}
      </Layer>
    ];
  }
}
ContextElement.propTypes = {
  data: PropTypes.object.isRequired,
  selectContext: PropTypes.func.isRequired,
  selectedContext: PropTypes.string
};



class PortElement extends React.Component {
  constructor() {
    super();
    this.state = {
      displayTextOnPart: null
    };

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.onMouseEnter = this.onMouseEnter.bind(this);
    this.onMouseLeave = this.onMouseLeave.bind(this);
  }

  onMouseEnter(index) {
    this.setState({displayTextOnPart: index});
  }

  onMouseLeave() {
    this.setState({displayTextOnPart: null})
  }

  render() {
    let parts = [];
    let portWidthOffset = (CELL_WIDTH + PORT_BODY_SHIFT) - PORT_WIDTH;
    for (let i in this.props.data.parts) {
      let part = this.props.data.parts[i];
      let x = part.x*(CELL_WIDTH+CELL_HGUTTER) + portWidthOffset;
      let y = part.fromY*CELL_HEIGHT;
      let width = PORT_WIDTH;
      let height = (part.toY - part.fromY)*CELL_HEIGHT;
      parts.push(<rect key={i}
        onMouseEnter={this.onMouseEnter.bind(this, i)} onMouseLeave={this.onMouseLeave}
        x={x} y={y} width={width} height={height} className="port-body" />);

      // add connecting line between different parts in different processes
      if (i != 0) {
        let prevPart = this.props.data.parts[i-1];
        let x1 = Math.min(part.x, prevPart.x)*(CELL_WIDTH+CELL_HGUTTER) + CELL_WIDTH/2 + 1;
        let x2 = Math.max(part.x, prevPart.x)*(CELL_WIDTH+CELL_HGUTTER) + portWidthOffset + PORT_WIDTH;
        parts.push(<line key={"l"+i} x1={x1} y1={y-0.5} x2={x2} y2={y-0.5} className="port-body" />);
      }
    }

    let lastPart = this.props.data.parts[this.props.data.parts.length-1];
    // if port already terminated, add mark
    if (this.props.data.exitReason) {
      let height = (this.props.data.exitReason == 'normal') ? 1 : CELL_HEIGHT/2;
      let x = lastPart.x*(CELL_WIDTH+CELL_HGUTTER) + portWidthOffset;
      let y = lastPart.toY*CELL_HEIGHT - height;
      let width = PORT_WIDTH;
      parts.push(<rect key="reason" x={x} y={y} width={width} height={height} className="exit-reason" />);
    }

    let textNode = null;
    if (this.state.displayTextOnPart) {
      const part = this.props.data.parts[this.state.displayTextOnPart];
      let x = part.x*(CELL_WIDTH+CELL_HGUTTER) + portWidthOffset;
      let y = part.fromY*CELL_HEIGHT - CELL_HEIGHT;
      textNode = <text x={x} y={y} className="label">
        {this.props.data.driverName}
      </text>;
    }

    return [
      <Layer key="text" name="text">
        {textNode}
      </Layer>,
      <Layer key="portBodies" name="portBodies">
        <g>{parts}</g>
      </Layer>
    ];
  }
}



class ScenarioView extends React.Component {
  constructor() {
    super();
    this.state = {
      viewportWidth: Math.max(document.documentElement.clientWidth, window.innerWidth || 0),
      viewportHeight: Math.max(document.documentElement.clientHeight, window.innerHeight || 0),
      highlightRange: null
    }

    // because new EcmaScript standard is poorly designed
    // we have to do bindings like that
    this.highlightRange = this.highlightRange.bind(this);
  }

  componentDidMount() {
    let context = null;
    if (this.props.match.params.context) {
      context = this.props.match.params.context.split('/').join(' ');
    }
    this.props.onInstanceIdChange(this.props.match.params.id, context);
  }

  componentWillReceiveProps(props) {
    const idChanged = this.props.match.params.id != props.match.params.id;
    const contextChanged = this.props.match.params.context != props.match.params.context;
    if (idChanged || contextChanged) {
      let context = null;
      if (this.props.match.params.context) {
        context = this.props.match.params.context.split('/').join(' ');
      }
      this.props.onInstanceIdChange(props.match.params.id, context);
    }
  }

  getChildContext() {
    return {getLayerNode: this.getLayerNode.bind(this)};
  }

  getLayerNode(key) {
    let ref = this.refs[key];
    if (!ref) {
      console.error("Unknown layer key: ", key);
      return null;
    }
    return ref;
  }

  highlightRange(range) {
    this.setState({highlightRange: range});
  }

  renderHighlightRange() {
    if (!this.state.highlightRange) {
      return <Layer name="highlightArea"></Layer>;
    }
    if (this.state.highlightRange.fromY == this.state.highlightRange.toY) {
      let y = this.state.highlightRange.fromY*CELL_HEIGHT - CELL_HEIGHT/2;
      let height = CELL_HEIGHT;
      return <Layer name="highlightArea">
        <rect x={-1000} y={y} width={2000} height={height} className="highlight-area" />
        <line x1={-1000} y1={y+CELL_HEIGHT/2} x2={1000} y2={y+CELL_HEIGHT/2} className="highlight-line" />
      </Layer>;
    } else {
      let y = this.state.highlightRange.fromY*CELL_HEIGHT;
      let height = (this.state.highlightRange.toY - this.state.highlightRange.fromY)*CELL_HEIGHT;
      return <Layer name="highlightArea">
        <rect x={-1000} y={y} width={2000} height={height} className="highlight-area highlight-line" />
      </Layer>;
    }
  }

  renderGrid() {
    // return null;
    let maxX = 1 + this.state.viewportWidth/(CELL_WIDTH+CELL_HGUTTER);
    let maxY = 1 + this.state.viewportHeight/CELL_HEIGHT;
    let lines = [];

    let minX = -Math.floor((SOURCE_PANEL_WIDTH+100)/(CELL_WIDTH+CELL_HGUTTER));
    let minY = -10;

    for (let i = minX; i < maxX; i++) {
      let xa = i*(CELL_WIDTH+CELL_HGUTTER);
      let xb = i*(CELL_WIDTH+CELL_HGUTTER) + CELL_WIDTH;
      lines.push(<line key={'ha-' + i} x1={xa+0.5} y1={minY*CELL_HEIGHT} x2={xa+0.5} y2={maxY*CELL_HEIGHT} style={{stroke: '#fee'}} />);
      lines.push(<line key={'hb-' + i} x1={xb-0.5} y1={minY*CELL_HEIGHT} x2={xb-0.5} y2={maxY*CELL_HEIGHT} style={{stroke: '#fee'}} />);
    }
    for (let i = minY; i < maxY; i++) {
      let y = i*CELL_HEIGHT;
      lines.push(<line key={'v-' + i} x1={minX*(CELL_WIDTH+CELL_HGUTTER)} y1={y+0.5} x2={maxX*(CELL_WIDTH+CELL_HGUTTER)} y2={y+0.5} style={{stroke: '#fee'}} />);
    }
    return lines;
  }

  render() {
    if (!this.props.tree) { return <div />; }

    let processes = [], spawns = [], links = [], mentions = [], points = [], contexts = [], ports = [];
    for (let pid in this.props.tree.processes) {
      processes.push(<ProcessElement key={pid} data={this.props.tree.processes[pid]} tracePid={this.props.tracePid} />);
    }
    for (let key in this.props.tree.spawns) {
      spawns.push(<SpawnElement key={key} data={this.props.tree.spawns[key]} />);
    }
    for (let key in this.props.tree.links) {
      links.push(<LinkElement key={key} data={this.props.tree.links[key]} />);
    }
    for (let key in this.props.tree.points) {
      points.push(<PointElement key={key} data={this.props.tree.points[key]} />);
    }
    for (let key in this.props.tree.mentions) {
      mentions.push(<MentionElement key={key} data={this.props.tree.mentions[key]} />);
    }
    for (let key in this.props.tree.contexts) {
      contexts.push(<ContextElement key={key} data={this.props.tree.contexts[key]} selectedContext={this.props.selectedContext} selectContext={this.props.selectContext} />);
    }
    for (let key in this.props.tree.ports) {
      ports.push(<PortElement key={key} data={this.props.tree.ports[key]} />);
    }

    let width = this.props.tree.width*(CELL_WIDTH+CELL_HGUTTER);
    let height = this.props.tree.height*CELL_HEIGHT;
// <g>{this.renderGrid()}</g>
    return <div className="ScenarioView">
      <SvgView padding={100} paddingLeft={SOURCE_PANEL_WIDTH+100} paddedWidth={width} paddedHeight={height}>
        

        {/* layers where dom is actually rendered */}
        <g ref="highlightArea"></g>
        <g ref="mentions"></g>
        <g ref="linksBehindProcesses"></g>
        <g ref="spawnsBehindProcesses"></g>
        <g ref="inactiveContextsLines"></g>
        <g ref="processes"></g>
        <g ref="inactiveContexts"></g>
        <g ref="portBodies"></g>
        <g ref="linksOnProcesses"></g>
        <g ref="spawnsOnProcesses"></g>
        <g ref="points"></g>
        <g ref="activeContexts"></g>
        <g ref="text"></g>
        <g ref="selection"></g>

        {/* entities, compose different parts on different layers */}
        <g>
          {processes}
          {spawns}
          {links}
          {mentions}
          {points}
          {contexts}
          {ports}
          {this.renderHighlightRange()}
        </g>

      </SvgView>
      <Link to="/" id="back-button">Back to list</Link>
      <SourcePanel width={SOURCE_PANEL_WIDTH} contexts={this.props.tree.contexts} selectedContext={this.props.selectedContext}
        events={this.props.shellEvents} prompt={this.props.shellPrompt} submitInput={this.props.submitShellInput}
        highlightRange={this.highlightRange} />
    </div>;
  }
}
ScenarioView.childContextTypes = {
  getLayerNode: PropTypes.func
};
ScenarioView.propTypes = {
  selectContext: PropTypes.func.isRequired,
  tracePid: PropTypes.func.isRequired,
  selectedContext: PropTypes.string
};

