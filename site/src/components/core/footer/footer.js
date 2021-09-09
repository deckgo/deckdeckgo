import React from 'react';

import {footer} from './footer.module.scss';

import {FormattedMessage} from 'react-intl';

import {LinkButton} from '../buttons/link-button';
import {Links} from '../links/links';

export const Footer = ({action = true, lang}) => {
  return (
    <footer className={`${footer} ${action ? 'action' : ''}`}>
      {renderAction()}

      <Links lang={lang} action={action}></Links>
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

        <LinkButton
          style={{marginTop: '1.25rem'}}
          targetUrl="https://app.deckdeckgo.com/editor"
          msgId="footer.write.presentation"
          color="primary"></LinkButton>
      </section>
    );
  }
};
