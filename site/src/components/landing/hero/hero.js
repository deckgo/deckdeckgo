import React from 'react';

import {FormattedMessage} from 'react-intl';

import {main, title, subtitle} from './hero.module.scss';

export const Hero = () => {
  return (
    <section>
      <div className={main}>
        <h1 className={title}>
          <FormattedMessage id="hero.title" />
        </h1>

        <section className={subtitle}>
          <FormattedMessage id="hero.subtitle" />
        </section>
      </div>
    </section>
  );
};
