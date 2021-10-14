import React from 'react';

import {FormattedMessage} from 'react-intl';

import {main, title, section, hero, subtitle} from './hero.module.scss';

export const FeaturesHero = () => {
  return <>{renderSectionHero()}</>;

  function renderSectionHero() {
    return (
      <section className={section}>
        <div className={`${main} ${hero}`}>
          <h1 className={title}>
            <FormattedMessage id="features.hero.title" />
          </h1>

          <section className={subtitle}>
            <FormattedMessage id="features.hero.subtitle" />
          </section>
        </div>
      </section>
    );
  }
};
