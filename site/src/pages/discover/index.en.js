import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';
import {Feed} from '../../components/discover/feed/feed';

import styles from '../../themes/templates/template.module.scss';
import discoverStyles from './discover.module.scss';

const DiscoverPage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <main className={`${styles.main} ${discoverStyles.main}`}>
          <h1>Discover</h1>

          <p>These are decks made with DeckDeckGo.</p>
        </main>
      </section>

      <Feed></Feed>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default DiscoverPage;
