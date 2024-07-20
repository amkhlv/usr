/* @refresh reload */
import { render } from 'solid-js/web';
import { Router, Route } from "@solidjs/router";

import './index.css';
import Brazil from './Brazil';
import Home from './Home';
import MathWriteups from './MathWriteups';
import Computing from './Computing'
import Contact from './Contact'

const root = document.getElementById('root');

if (import.meta.env.DEV && !(root instanceof HTMLElement)) {
  throw new Error(
    'Root element not found. Did you forget to add it to your index.html? Or maybe the id attribute got misspelled?',
  );
}

const App = (props) => (
  <>
    <nav>
      <a href="/">Home</a>&nbsp;
      <a href="/research">Math</a>&nbsp;
      <a href="/brazil">Brazil</a>&nbsp;
      <a href="/computing">Computing</a>&nbsp;
      <a href="/contact">Contact</a>
    </nav>
    {props.children}
  </>
);

render(
  () => (
    <Router root={App}>
      <Route path="/" component={Home} />
      <Route path="/research" component={MathWriteups} />
      <Route path="/brazil" component={Brazil} />
      <Route path="/computing" component={Computing} />
      <Route path="/contact" component={Contact} />
    </Router>

  ),
  root);
