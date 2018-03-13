// this element provides rules for generating layout


export default {
  produceElements
};


// this function extracts all request elements from delta
export function produceElements(delta) {
  const result = [];

  if (delta['plug:requests']) {
    for (const key in delta['plug:requests']) {
      const req = delta['plug:requests'][key];
      result.push({
        key: key,
        x1: req.Pid,
        y1: req.StartedAt,
        y2: req.StoppedAt,
      });
    }
  }

  return result;
};
