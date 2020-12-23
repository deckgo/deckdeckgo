import React from 'react';

import {FormattedMessage} from 'react-intl';

import styles from './hero.module.scss';

export const Hero = () => {
  return (
    <section>
      <main className={styles.main}>
        <h1 className={styles.title}>
          <FormattedMessage id="hero.title" />
        </h1>

        <section className={styles.subtitle}>
          <FormattedMessage id="hero.subtitle" />
        </section>
      </main>
    </section>
  );
};
