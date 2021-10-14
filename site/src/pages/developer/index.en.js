import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';

import {main} from '../../themes/templates/template.module.scss';

const DeveloperPage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <div className={main}>
          <h1>Developer</h1>

          <h2>You know what's cool?</h2>

          <p>
            The core of DeckDeckGo and many other libraries we have developed could be used separately with or without in any modern
            framework. These are the exact same libraries we are using to develop this application respectively our platform. It means that
            each time we are going to improve it, the related open sourced library will inherit these improvements too{' '}
            <span role="img" aria-hidden={true}>
              🚀
            </span>
            .
          </p>

          <h3>Cherry on top</h3>

          <p>
            We hope that you do like to edit, present and publish your talk with our application. But if you are a developer and rather like
            to use <strong>Html</strong> to compose your slides, build and deploy your deck yourself, we do provide a Cli, starter kit and
            even a full <a href="https://docs.deckdeckgo.com">documentation</a>.
          </p>

          <p>If you would like to follow that path, run the following command in your terminal to begin your journey.</p>

          <div style={{display: 'flex', justifyContent: 'center'}}>
            <deckgo-highlight-code language="bash">
              <code slot="code">npm init deckdeckgo</code>
            </deckgo-highlight-code>
          </div>
        </div>
      </section>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default DeveloperPage;
