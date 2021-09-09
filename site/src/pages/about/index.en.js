import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';

import {main} from '../../themes/templates/template.module.scss';

const AboutPage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <main className={main}>
          <h1>About DeckDeckGo</h1>

          <h3>Edit anywhere, display everywhere</h3>

          <p>
            DeckDeckGo aims to be the open source web editor for presentations. It enables anyone with any type of devices (desktop, mobile
            or tablets) to easily create, present and share presentations for free.
          </p>

          <p>
            What makes it different{' '}
            <span role="img" aria-hidden={true}>
              🤔
            </span>
            ? Every presentations published with DeckDeckGo are standalone <strong>Progressive Web Apps</strong>{' '}
            <span role="img" aria-hidden={true}>
              🚀
            </span>
            .
          </p>

          <p>
            Moreover, it would be incredible if it would become an online community for sharing presentations, slides and talks about your
            interests and ideas.
          </p>

          <p>
            DeckDeckGo is created with passion since 2019 in Zürich
            <span role="img" aria-hidden={true}>
              🇨🇭
            </span>
            .
          </p>
        </main>
      </section>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default AboutPage;
