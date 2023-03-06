import React from 'react';
import ReactDOM from 'react-dom/client';

function App() {
  const myFirstElement = <h1>Hello!</h1>

  const root = ReactDOM.createRoot(document.getElementById('root'));

  root.render(myFirstElement);

  class Card {
    construct(ch,en,py) {
      this.chinese = ch;
      this.english = en;
      this.pinyin = py;
    }

    flip() {
      return chinese + pinyin;
    }
  }
}
