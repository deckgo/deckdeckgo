import React from 'react';

import {FormattedMessage} from 'react-intl';

import {main, title, section, hero, quote, subtitle} from './hero.module.scss';

export const FeaturesHero = () => {

  return (
    <>
      {renderSectionHero()}

      {renderQuote()}
    </>
  );

  function renderSectionHero() {
    return (
      <section className={section}>
        <main className={`${main} ${hero}`}>
          <h1 className={title}>
            <FormattedMessage id="features.hero.title" />
          </h1>

          <section className={subtitle}>
            <FormattedMessage id="features.hero.subtitle" />
          </section>
        </main>
      </section>
    );
  }

  function renderQuote() {
    return (
      <section>
        <main className={`${main} ${quote}`}>
          <blockquote>
            <span>❝</span>
            <p>
              <FormattedMessage id="enterprise.hero.quote" />
            </p>
          </blockquote>

          <h2>
            <FormattedMessage id="enterprise.hero.templates.title" />
          </h2>

          <div>
            <p>
              <FormattedMessage id="enterprise.hero.templates.unlike" />
            </p>

            <p>
              <FormattedMessage id="enterprise.hero.templates.custom" />
            </p>
          </div>

          <div>
            <p>
              <FormattedMessage id="enterprise.hero.templates.together" />
            </p>

            <p>
              <FormattedMessage id="enterprise.hero.templates.uptodate" />
            </p>
          </div>
        </main>
      </section>
    );
  }
};
