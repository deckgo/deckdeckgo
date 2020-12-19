import React from 'react';

import styles from './footer.module.scss';

import {FormattedMessage} from 'react-intl';

import {LinkButton} from '../buttons/link-button';
import {Link} from 'gatsby';

export const Footer = ({action = true, lang}) => {
  return (
    <footer className={`${styles.footer} ${action ? 'action' : ''}`}>
      {renderAction()}

      {renderFooter()}
    </footer>
  );

  function renderAction() {
    if (!action) {
      return undefined;
    }

    return (
      <section>
        <h3>
          <FormattedMessage id="footer.start.now" />
        </h3>

        <LinkButton style={{marginTop: '1.25rem'}} targetUrl="https://app.deckdeckgo.com" msgId="footer.write.presentation" color="primary"></LinkButton>
      </section>
    );
  }

  function renderFooter() {
    return (
      <div className={`${styles.links} ${action ? 'action' : ''}`}>
        <section>
          <p className={styles.sectionTitle}>DeckDeckGo</p>

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

        <section>
          <p className={styles.sectionTitle}>
            <FormattedMessage id="footer.link.title.interact" />
          </p>

          <a href="https://app.deckdeckgo.com/poll" rel="noopener norefferer">
            <FormattedMessage id="footer.link.poll" />
          </a>

          <a href="https://deckdeckgo.app" rel="noopener norefferer">
            <FormattedMessage id="footer.link.remote" />
          </a>
        </section>

        <section>
          <p className={styles.sectionTitle}>
            <FormattedMessage id="footer.link.title.developers" />
          </p>

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
          <p className={styles.sectionTitle}>
            <FormattedMessage id="footer.link.title.terms" />
          </p>

          <Link to={`/terms`}>
            <FormattedMessage id="footer.link.terms" />
          </Link>

          <Link to={`/privacy`}>
            <FormattedMessage id="footer.link.privacy" />
          </Link>
        </section>

        <div className={styles.social}>
          <a href="https://twitter.com/deckdeckgo" rel="noopener norefferer" aria-label="Twitter">
            <img loading="lazy" src="/assets/icons/ionicons/twitter.svg" aria-hidden="true" alt="" style={{width: '2rem', padding: '0.45rem'}} />
          </a>
          <a href="https://github.com/deckgo" rel="noopener noreferrer" aria-label="GitHub">
            <img loading="lazy" src="/assets/icons/ionicons/github.svg" aria-hidden="true" alt="" style={{width: '2rem', padding: '0.45rem'}} />
          </a>
          <a
            href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY"
            rel="noopener noreferrer"
            aria-label="Slack">
            <img loading="lazy" src="/assets/icons/ionicons/slack.svg" aria-hidden="true" alt="" style={{width: '2rem', padding: '0.45rem'}} />
          </a>
        </div>
      </div>
    );
  }
};
