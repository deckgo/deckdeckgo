import React from 'react';

import {FormattedMessage} from 'react-intl';

import {Link} from 'gatsby';

import {main, article, tag} from './plan.module.scss'
import { LinkButton } from "../../core/buttons/link-button";

export const Plan = ({lang}) => {
  return (
    <section>
      <main className={main}>
        {renderCommunity()}

        {renderSponsorhip()}

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
          <span>0</span>
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

  function renderSponsorhip() {
    return (
      <article className={article}>
        <h2>
          <FormattedMessage id="pricing.sponsor.title" />
        </h2>

        <p><FormattedMessage id="pricing.sponsor.mvp" /></p>

        <p className={tag}>
        </p>

        <aside>
          <h4>
            <FormattedMessage id="pricing.sponsor.includes" />
          </h4>

          <ul>
            <li>
              <FormattedMessage
                id="pricing.sponsor.featured"
                values={{
                  openCollectiveLink: (
                    <a href="https://opencollective.com/deckdeckgo" rel="noopener noreferrer" style={{textDecoration: 'underline'}}>
                      Open Collective
                    </a>
                  ),
                  githubLink: (
                    <a href="https://github.com/deckgo/deckdeckgo" rel="noopener noreferrer" style={{textDecoration: 'underline'}}>
                      GitHub
                    </a>
                  ),
                }}
              />
            </li>
            <li>
              <FormattedMessage id="pricing.sponsor.voucher" />
            </li>
          </ul>
        </aside>

        <LinkButton
          targetUrl="https://opencollective.com/deckdeckgo#category-CONTRIBUTE"
          msgId="pricing.sponsor.title"
          color="primary"></LinkButton>
      </article>
    );
  }

  function renderEnterprise() {
    return <article className={article}>TODO: Enterprise</article>;
  }
};
