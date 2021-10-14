import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';

import {main} from '../../themes/templates/template.module.scss';

const ContactPage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <div className={main}>
          <h1>Contact</h1>

          <p>We would love to hear from you, ping us!</p>

          <p>
            Email:{' '}
            <a href="mailto:hello@deckdeckgo.com" rel="noopener noreferrer">
              hello@deckdeckgo.com
            </a>
          </p>

          <p>
            Twitter:{' '}
            <a href="https://twitter.com/deckdeckgo" rel="noopener noreferrer">
              @deckdeckgo
            </a>
          </p>

          <p>
            Or join us on our dedicated{' '}
            <a
              href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY"
              rel="noopener noreferrer">
              Slack
            </a>{' '}
            channel.
          </p>

          <p>
            If you would like to contribute, that would be really awesome! For feature requests, issues or even better Pull Requests, find
            us on{' '}
            <a href="https://github.com/deckgo/deckdeckgo" rel="noopener noreferrer">
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

export default ContactPage;
