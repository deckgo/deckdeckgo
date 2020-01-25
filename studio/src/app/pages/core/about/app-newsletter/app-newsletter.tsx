import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-newsletter',
  styleUrl: 'app-newsletter.scss'
})
export class AppNewsletter {
  render() {
    return [
      <app-navigation presentation={true}></app-navigation>,
      <ion-content class="ion-padding">
        <main class="ion-padding">
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
                <input
                  type="email"
                  value=""
                  name="EMAIL"
                  class="required email"
                  id="mce-EMAIL"
                  placeholder="Email"
                  style={{width: '100%', padding: '8px 0', 'text-indent': '2%', border: '1px solid #ABB0B2', 'border-radius': '4px'}}
                />
              </div>
              <div id="mce-responses">
                <div class="response" id="mce-error-response" style={{display: 'none'}}></div>
                <div class="response" id="mce-success-response" style={{display: 'none'}}></div>
              </div>
              <div style={{position: 'absolute', left: '-50000px'}} aria-hidden="true">
                <input type="text" name="b_9b5f83c6cb1006ea8f7225ccb_264e8d156c" tabindex="-1" value="" />
              </div>
              <div>
                <ion-button type="submit" color="primary" shape="round" id="mc-embedded-subscribe">
                  Subscribe
                </ion-button>
              </div>
            </div>
          </form>
        </main>
      </ion-content>
    ];
  }
}
