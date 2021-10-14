import * as React from 'react';

import {FormattedMessage} from 'react-intl';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';
import {Feed} from '../../components/discover/feed/feed';

import {main} from '../../themes/templates/template.module.scss';
import {main as discoverStyles} from './discover.module.scss';

const DiscoverPage = (props) => {
  return (
    <Layout location={props.location} sticky={false}>
      <section>
        <div className={`${main} ${discoverStyles}`}>
          <h1>
            <FormattedMessage id="discover.title" />
          </h1>

          <p>
            <FormattedMessage id="discover.made.with.deckdeckdo" />
          </p>
        </div>
      </section>

      <Feed></Feed>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default DiscoverPage;
