import React, {forwardRef, useImperativeHandle, useState} from 'react';
import {Link} from 'gatsby';

import {FormattedMessage} from 'react-intl';

import styles from './menu.module.scss';

export const Menu = forwardRef((props, ref) => {
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
      <Link to="/">
        <h1>Home</h1>
      </Link>
    </div>
  );
});
