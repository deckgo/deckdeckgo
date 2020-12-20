import React from 'react';

import {FormattedMessage} from 'react-intl';

import styles from './card.module.scss';

export const Card = () => {
  return (
    <figure className={styles.figure}>
      <img
        className={styles.screenshot}
        width={1024}
        height={576}
        loading="lazy"
        src="https://firebasestorage.googleapis.com/v0/b/deckdeckgo-studio-prod.appspot.com/o/NUr0tNvY7dhIA5qO5Kx9wtyL5Ne2%2Fpresentations%2Foccc---ti-99-and-the-8bit-internet%2Fdeckdeckgo.png?alt=media"
      />
    </figure>
  );
};
