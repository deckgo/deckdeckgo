import React from 'react';

import {FormattedMessage} from 'react-intl';

import {main, title, section, discover, hero, quote, subtitle} from './hero.module.scss';

import {ActionButton} from '../../core/buttons/action-button';

export const EnterpriseHero = () => {
  const scrollTo = () => {
    const contact = document.querySelector('form');

    if (!contact) {
      return;
    }

    contact.scrollIntoView({
      behavior: 'smooth'
    });
  };

  return (
    <>
      {renderSectionHero()}

      {renderQuote()}
    </>
  );

  function renderSectionHero() {
    return (
      <section className={section}>
        <div className={`${main} ${hero}`}>
          <h1 className={title}>
            <FormattedMessage id="enterprise.hero.title" />
          </h1>

          <section className={subtitle}>
            <FormattedMessage id="enterprise.hero.subtitle" />
          </section>

          <ActionButton type="button" msgId="enterprise.hero.get.in.touch" color="primary" action={scrollTo}></ActionButton>
        </div>
      </section>
    );
  }

  function renderQuote() {
    return (
      <section>
        <div className={`${main} ${quote}`}>
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

            <button type="button" className={discover} onClick={scrollTo}>
              <FormattedMessage id="enterprise.hero.get.in.touch" />{' '}
              <svg xmlns="http://www.w3.org/2000/svg" width="1rem" height="1rem" viewBox="0 0 512 512">
                <polyline
                  points="268 112 412 256 268 400"
                  style={{fill: 'none', stroke: 'currentColor', strokeLinecap: 'round', strokeLinejoin: 'round', strokeWidth: '48px'}}
                />
                <line
                  x1="392"
                  y1="256"
                  x2="100"
                  y2="256"
                  style={{fill: 'none', stroke: 'currentColor', strokeLinecap: 'round', strokeLinejoin: 'round', strokeWidth: '48px'}}
                />
              </svg>
            </button>
          </div>
        </div>
      </section>
    );
  }
};
