// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
// import "phoenix_html"

// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".

// import socket from "./socket"



import ReactDOM from 'react-dom';
import React from 'react';
import { BrowserRouter, Route } from 'react-router-dom';
import MobxDevTools from 'mobx-react-devtools'
import { Provider } from 'mobx-react';
import { extendObservable } from 'mobx';

import Store from './Store';
import InstancesPage from './InstancesPage';



document.addEventListener("DOMContentLoaded", function() {
  // assign to global var only for debug in console
  // never refer to this global var in actual code
  window.VisionStore = new Store();

  const routes = (
    <div>
      <Provider store={window.VisionStore}>
        <BrowserRouter>
          <Route path="/" component={InstancesPage} />
        </BrowserRouter>
      </Provider>
      {/*<MobxDevTools />*/}
    </div>
  );

  ReactDOM.render(routes, document.getElementById('container'));
});
