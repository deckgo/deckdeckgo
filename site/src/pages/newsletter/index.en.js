import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';

import {main} from '../../themes/templates/template.module.scss';
import {ActionButton} from '../../components/core/buttons/action-button';

const NewsletterPage = (props) => {
  return (
    <Layout location={props.location}>
      <section>
        <main className={main}>
          <h1>Join our Newsletter</h1>

          <p>Sign up to receive time to time an email on the latest DeckDeckGo updates, features, and news!</p>

          <form
            action="https://deckdeckgo.us20.list-manage.com/subscribe/post?u=9b5f83c6cb1006ea8f7225ccb&amp;id=3a584996fb"
            method="post"
            id="mc-embedded-subscribe-form"
            name="mc-embedded-subscribe-form"
            target="_blank"
            novalidate>
            <div id="mc_embed_signup_scroll">
              <div margin-top margin-bottom>
                <input type="email" defaultValue="" name="EMAIL" id="mce-EMAIL" placeholder="Email" />
              </div>
              <div id="mce-responses">
                <div id="mce-error-response" style={{display: 'none'}}></div>
                <div id="mce-success-response" style={{display: 'none'}}></div>
              </div>
              <div style={{position: 'absolute', left: '-50000px'}} aria-hidden="true">
                <input type="text" name="b_9b5f83c6cb1006ea8f7225ccb_264e8d156c" tabindex="-1" value="" />
              </div>

              <ActionButton type="submit" msgId="common.subscribe" color="primary" style={{marginTop: '1.45rem'}}></ActionButton>
            </div>
          </form>
        </main>
      </section>

      <Footer action={false} lang="en" />
    </Layout>
  );
};

export default NewsletterPage;
