import React, {forwardRef, useImperativeHandle, useState} from 'react';
import {Link} from 'gatsby';

import {FormattedMessage} from 'react-intl';

import {menu, open as openStyle} from './menu.module.scss';

import {Links} from '../links/links';

export const Menu = forwardRef(({lang}, ref) => {
  useImperativeHandle(ref, () => ({
    open() {
      display();
    }
  }));

  const [open, setOpen] = useState(false);

  const hide = () => {
    setOpen(false);
  };

  const display = () => {
    setOpen(true);
  };

  return (
    <div role="button" tabIndex="0" className={`${menu} ${open ? `${openStyle}` : ''}`} onClick={() => hide()} onKeyDown={() => hide()}>
      <img
        loading="lazy"
        src="/assets/icons/ionicons/close.svg"
        aria-hidden="true"
        alt=""
        style={{width: '3rem', padding: '0.45rem', position: 'fixed', top: '0', left: '0', margin: '15px 0 0 20px'}}
      />
      <Link to={`/${lang}/`}>
        <h2>DeckDeckGo</h2>
      </Link>

      <Link to={`/${lang}/features`}>
        <h2>
          <FormattedMessage id="nav.features" />
        </h2>
      </Link>

      <Link to={`/${lang}/discover`}>
        <h2>
          <FormattedMessage id="nav.discover" />
        </h2>
      </Link>

      <Link to={`/${lang}/pricing`}>
        <h2>
          <FormattedMessage id="nav.pricing" />
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

      <a href="https://app.deckdeckgo.com/editor" rel="noopener noreferrer">
        <h2>
          <FormattedMessage id="nav.write.presentation" />
        </h2>
      </a>

      <Links lang={lang} action={false} display="flex"></Links>
    </div>
  );
});
