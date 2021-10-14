import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';

import {main} from '../../themes/templates/template.module.scss';

const OpenSourcePage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <div className={main}>
          <h1>Open Source</h1>

          <p>
            DeckDeckGo is <strong>open source</strong>. This art of licencing is the DNA of the project. Beside its primary goal, we hope
            that this platform will help us become better programmers as we are betting on the web and modern technologies using it as a
            wonderful training. We also hope that by sharing our code it will be beneficial for the community.
          </p>

          <h2>Licence</h2>

          <p>
            The platform and its applications are licensed under the AGPL v3 (or later) licence. Several separate components are licensed
            under MIT licence. The licence displayed in each projects (see their README.md) is decisive.
          </p>

          <h2>Repo</h2>

          <p>
            The open source code of DeckDeckGo is available on&nbsp;
            <a href="http://github.com/deckgo/deckdeckgo" rel="noopener noreferrer">
              GitHub
            </a>
            .
          </p>
        </div>
      </section>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default OpenSourcePage;
