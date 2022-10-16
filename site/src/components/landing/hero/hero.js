import React from 'react';

import {FormattedMessage} from 'react-intl';

import {main, title, subtitle} from './hero.module.scss';

export const Hero = () => {
  return (
    <section>
      <div className={main}>
        <h1 className={title}>
            Bye bye DeckDeckGo 👋
        </h1>

        <section className={subtitle}>
            It has been a fun ride but DeckDeckGo is now <strong>deprecated</strong>!
        </section>
      </div>
    </section>
  );
};
