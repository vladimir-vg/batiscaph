import React from 'react';
import { Link } from 'react-router-dom';



const pidRe = /<\d+\.\d+\.\d+>/g;

// this function takes callback, text,
// and feeds tokens to callback.
// tokens that might be useful, like pids (to turn them into links)
function eachToken(text, { onText, onPid }) {
  let prevIndex = 0;
  let match = pidRe.exec(text);

  while (match != null) {
    if (prevIndex != match.index) {
      onText(text.slice(prevIndex, match.index), prevIndex);
    }
    onPid(match[0], match.index);

    prevIndex = match.index + match[0].length;
    match = pidRe.exec(text);
  }

  if (prevIndex != text.length) {
    onText(text.slice(prevIndex, text.length), prevIndex);
  }
}



function addLinksIntoText(text, { instanceId }) {
  const result = [];
  eachToken(text, {
    onText: (text, i) => { result.push(<span key={i}>{text}</span>) },
    onPid: (pid, i) => {
      const path = `/instances/${instanceId}/process-info/${pid}`;
      result.push(<Link key={i} to={path}>{pid}</Link>);
    },
  });
  return result;
}



export default {
  addLinksIntoText
};