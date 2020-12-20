import React from 'react';

import {FormattedMessage} from 'react-intl';

import styles from './feed.module.scss';
import {Card} from '../card/card';

export const Feed = () => {
  return (
    <section>
      <main className={styles.main}>
        <Card></Card>

        <Card></Card>

        <Card></Card>

        <Card></Card>

        <Card></Card>

        <Card></Card>
      </main>
    </section>
  );
};
