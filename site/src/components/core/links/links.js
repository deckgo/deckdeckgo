import React from 'react';
import {Link} from 'gatsby';

import {FormattedMessage} from 'react-intl';

import {links, sectionTitle, social} from './links.module.scss';

export const Links = ({lang, action, display = 'grid'}) => {
  return (
    <div className={`${links} ${action ? 'action' : ''} ${display}`}>
      <section>
        {display === 'grid' ? <p className={sectionTitle}>DeckDeckGo</p> : undefined}

        <Link to={`/${lang}/about`}>
          <FormattedMessage id="footer.link.about" />
        </Link>

        <Link to={`/${lang}/team`}>
          <FormattedMessage id="footer.link.team" />
        </Link>

        <Link to={`/${lang}/newsletter`}>
          <FormattedMessage id="footer.link.newsletter" />
        </Link>

        <Link to={`/${lang}/contact`}>
          <FormattedMessage id="footer.link.contact" />
        </Link>

        <Link to={`/${lang}/press`}>
          <FormattedMessage id="footer.link.press" />
        </Link>

        <Link to={`/${lang}/faq`}>
          <FormattedMessage id="footer.link.faq" />
        </Link>
      </section>

      {display === 'grid' ? (
        <section>
          <p className={sectionTitle}>
            <FormattedMessage id="footer.link.title.product" />
          </p>

          <Link to={`/${lang}/features`}>
            <FormattedMessage id="footer.link.features" />
          </Link>

          <Link to={`/${lang}/discover`}>
            <FormattedMessage id="footer.link.discover" />
          </Link>

          <Link to={`/${lang}/pricing`}>
            <FormattedMessage id="footer.link.pricing" />
          </Link>

          <Link to={`/${lang}/enterprise`}>
            <FormattedMessage id="footer.link.enterprise" />
          </Link>
        </section>
      ) : undefined}

      <section>
        {display === 'grid' ? (
          <p className={sectionTitle}>
            <FormattedMessage id="footer.link.title.developers" />
          </p>
        ) : undefined}

        <Link to={`/${lang}/opensource`}>
          <FormattedMessage id="footer.link.opensource" />
        </Link>

        <Link to={`/${lang}/services`}>
          <FormattedMessage id="footer.link.services" />
        </Link>

        <Link to={`/${lang}/developer`}>
          <FormattedMessage id="footer.link.developer" />
        </Link>
      </section>

      <section>
        {display === 'grid' ? (
          <p className={sectionTitle}>
            <FormattedMessage id="footer.link.title.legal" />
          </p>
        ) : undefined}

        <Link to={`/terms`}>
          <FormattedMessage id="footer.link.terms" />
        </Link>

        <Link to={`/privacy`}>
          <FormattedMessage id="footer.link.privacy" />
        </Link>
      </section>

      <div className={social}>
        <a href="https://twitter.com/deckdeckgo" rel="noopener norefferer" aria-label="Twitter">
          <img
            loading="lazy"
            src="/assets/icons/ionicons/twitter.svg"
            aria-hidden="true"
            alt=""
            style={{width: '2rem', padding: '0.45rem'}}
          />
        </a>
        <a href="https://github.com/deckgo/deckdeckgo" rel="noopener noreferrer" aria-label="GitHub">
          <img
            loading="lazy"
            src="/assets/icons/ionicons/github.svg"
            aria-hidden="true"
            alt=""
            style={{width: '2rem', padding: '0.45rem'}}
          />
        </a>
        <a
          href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY"
          rel="noopener noreferrer"
          aria-label="Slack">
          <img
            loading="lazy"
            src="/assets/icons/ionicons/slack.svg"
            aria-hidden="true"
            alt=""
            style={{width: '2rem', padding: '0.45rem'}}
          />
        </a>
      </div>
    </div>
  );
};
