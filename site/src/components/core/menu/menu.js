import React, {forwardRef, useImperativeHandle, useState} from 'react';
import {Link} from 'gatsby';

import {FormattedMessage} from 'react-intl';

import styles from './menu.module.scss';
import {Links} from '../links/links';

export const Menu = forwardRef(({lang}, ref) => {
  useImperativeHandle(ref, () => ({
    open() {
      display();
    },
  }));

  const [open, setOpen] = useState(false);

  const hide = () => {
    setOpen(false);
  };

  const display = () => {
    setOpen(true);
  };

  return (
    <div role="button" tabIndex="0" className={`${styles.menu} ${open ? `${styles.open}` : ''}`} onClick={() => hide()} onKeyDown={() => hide()}>
      <Link to={`/${lang}/`}>
        <h2>DeckDeckGo</h2>
      </Link>

      <Link to={`/${lang}/discover`}>
        <h2>
          <FormattedMessage id="nav.discover" />
        </h2>
      </Link>

      <Link to={`/${lang}/enterprise`}>
        <h2>
          <FormattedMessage id="nav.enterprise" />
        </h2>
      </Link>

      <a href="https://app.deckdeckgo.com/signin" rel="noopener noreferrer">
        <h2>
          <FormattedMessage id="nav.signin" />
        </h2>
      </a>

      <a href="https://app.deckdeckgo.com" rel="noopener noreferrer">
        <h2>
          <FormattedMessage id="nav.write.presentation" />
        </h2>
      </a>

      <Links lang={lang} action={false} display="flex"></Links>
    </div>
  );
});
