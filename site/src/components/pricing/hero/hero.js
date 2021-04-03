import React from 'react';

import {FormattedMessage} from 'react-intl';

import {main, title, section, hero, subtitle} from './hero.module.scss';

export const PricingHero = () => {

  return (
    <section className={section}>
      <main className={`${main} ${hero}`}>
        <h1 className={title}>
          <FormattedMessage id="pricing.hero.title" />
        </h1>

        <section className={subtitle}>
          <FormattedMessage id="pricing.hero.subtitle" />
        </section>
      </main>
    </section>
  );

};
