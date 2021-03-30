import React from 'react';

import {FormattedMessage} from 'react-intl';

import {main, article, tag} from './plan.module.scss';
import {Link} from 'gatsby';

export const Plan = ({lang}) => {
  return (
    <section>
      <main className={main}>
        {renderCommunity()}

        {renderEnterprise()}
      </main>
    </section>
  );

  function renderCommunity() {
    return (
      <article className={article}>
        <h2>
          <FormattedMessage id="pricing.community.title" />
        </h2>

        <p>
          <FormattedMessage id="pricing.community.subtitle" />
        </p>

        <p className={tag}>
          <span>$</span>
          <h2>0</h2>
        </p>

        <p>
          <FormattedMessage
            id="pricing.community.free"
            values={{
              sponsorshipLink: (
                <a href="https://opencollective.com/deckdeckgo#category-CONTRIBUTE" rel="noopener noreferrer" style={{textDecoration: 'underline'}}>
                  <FormattedMessage id="pricing.community.sponsorship" />
                </a>
              ),
            }}
          />
        </p>

        <aside>
          <h4>
            <FormattedMessage id="pricing.community.includes" />
          </h4>

          <ul>
            <li>
              <FormattedMessage id="pricing.community.feature.pwa" />
            </li>
            <li>
              <FormattedMessage id="pricing.community.feature.poll" />
            </li>
            <li>
              <FormattedMessage id="pricing.community.feature.templates" />
            </li>
            <li>
              <FormattedMessage id="pricing.community.feature.unsplash" />
            </li>
            <li>
              <FormattedMessage id="pricing.community.feature.youtube" />
            </li>
            <li>
              <FormattedMessage id="pricing.community.feature.code" />
            </li>
            <li>
              <FormattedMessage id="pricing.community.feature.remote" />
            </li>
            <li>
              <FormattedMessage id="pricing.community.feature.offline" />
            </li>
            <li>
              <FormattedMessage id="pricing.community.feature.github" />
            </li>
            <li>
              <FormattedMessage id="pricing.community.feature.figma" />
            </li>
            <li>
              <FormattedMessage
                id="pricing.community.feature.more"
                values={{
                  featuresLink: (
                    <Link to={`/${lang}/features/`} style={{textDecoration: 'underline'}}>
                      <FormattedMessage id="pricing.community.features" />
                    </Link>
                  ),
                }}
              />
            </li>
          </ul>
        </aside>
      </article>
    );
  }

  function renderEnterprise() {
    return <article className={article}>Enterprise</article>;
  }
};
