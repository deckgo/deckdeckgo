import React from 'react';

import {FormattedMessage} from 'react-intl';

import {Link} from 'gatsby';

import {main, article, tag} from './plan.module.scss';
import {LinkButton} from '../../core/buttons/link-button';

export const Plan = ({lang}) => {
  return (
    <section>
      <div className={main}>
        {renderCommunity()}

        {renderEnterprise()}
      </div>
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
          <span>0</span>
        </p>

        <p>
          <FormattedMessage
            id="pricing.community.free"
            values={{
              sponsorshipLink: (
                <a href="https://opencollective.com/deckdeckgo#category-CONTRIBUTE" rel="noopener noreferrer">
                  <FormattedMessage id="pricing.community.sponsorship" />
                </a>
              )
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
              <FormattedMessage id="pricing.community.feature.figma" />
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
              <FormattedMessage
                id="pricing.community.feature.more"
                values={{
                  featuresLink: (
                    <Link to={`/${lang}/features/`}>
                      <FormattedMessage id="pricing.community.features" />
                    </Link>
                  )
                }}
              />
            </li>
          </ul>
        </aside>

        <LinkButton
          targetUrl="https://opencollective.com/deckdeckgo#category-CONTRIBUTE"
          msgId="pricing.sponsor.action"
          color="primary"></LinkButton>
      </article>
    );
  }

  function renderEnterprise() {
    return (
      <article className={article}>
        <h2>
          <FormattedMessage id="pricing.enterprise.title" />
        </h2>

        <p>
          <FormattedMessage id="enterprise.hero.title" />.
        </p>

        <p className={tag} aria-hidden={true}></p>

        <p aria-hidden={true}> </p>

        <aside>
          <h4>
            <FormattedMessage id="pricing.enterprise.includes" />
          </h4>

          <ul>
            <li>
              <FormattedMessage id="features.edit.unbreakable.title" />
            </li>
            <li>
              <FormattedMessage id="features.main.privatelinks.title" />
            </li>
            <li>
              <FormattedMessage id="features.main.pdf.title" />
            </li>
            <li>
              <FormattedMessage id="features.edit.customfonts.title" />
            </li>
            <li>
              <FormattedMessage id="features.enterprise.housing.content" />
            </li>
            <li>
              <FormattedMessage id="features.enterprise.domain.content" />
            </li>
            <li>
              <FormattedMessage id="features.enterprise.uptodate.title" />
            </li>
            <li>
              <FormattedMessage id="features.enterprise.archive.content" />
            </li>
            <li>
              <FormattedMessage id="features.enterprise.collaborate.content" />
            </li>
            <li>
              <FormattedMessage id="features.enterprise.library.content" />
            </li>
            <li>
              <FormattedMessage id="features.enterprise.support.content" />
            </li>
          </ul>
        </aside>

        <LinkButton targetUrl={`/${lang}/enterprise#contact`} msgId="pricing.enterprise.contact" color="primary-outline"></LinkButton>
      </article>
    );
  }
};
