import './App.css';
import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css';
import Game from './page/Game';

const root = ReactDOM.createRoot(
  document.getElementById('root') as HTMLElement
);
root.render(<Game />);

