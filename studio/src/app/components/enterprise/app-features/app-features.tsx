import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-features',
  styleUrl: 'app-features.scss',
  shadow: false,
})
export class AppFeatures {
  render() {
    return (
      <section class="features">
        <h2>Main features</h2>

        <article>
          <h3>Edit anywhere</h3>

          <p>The editor is available anywhere, it is a Progressive Web Apps!</p>
        </article>

        <article>
          <h3>Showcase everywhere</h3>

          <p>Presentations work on any device. These are published are standalone apps.</p>
        </article>

        <article>
          <h3>Privacy per default</h3>

          <p>Your presentations and content are private. You can choose to share them publicly anytime, but that's your choice.</p>
        </article>

        <article>
          <h3>Share standalone apps</h3>

          <p>Your presentation are published as standalone Progressive Web Apps. It only needs a link to share them.</p>
        </article>

        <article>
          <h3>
            Share privately <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
          </h3>

          <p>Create private links to restrain the access to your published content.</p>
        </article>

        <article>
          <h3>
            Export to PDF <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
          </h3>

          <p>If you rather like PDF over apps.</p>
        </article>

        <article>
          <h3>Polls</h3>

          <p>Engage your audience in real time. Involve them to contribute to your presentations with their smartphones and show the results live.</p>
        </article>

        <article>
          <h3>Offline</h3>

          <p>Present and edit your presentation offline.</p>
        </article>

        <h2>Editing</h2>

        <article>
          <h3>
            Unbreakable templates <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
          </h3>

          <p>Distribute and keep up-to-date your corporate identity and design across all your company's presentations.</p>
        </article>

        <article>
          <h3>Highlighted code</h3>

          <p>Show syntax highlighted code in beautiful terminal cards.</p>
        </article>

        <article>
          <h3>Embed your products</h3>

          <p>If your products are made with the web too, embed these directly in your slides.</p>
        </article>

        <article>
          <h3>Cloud library</h3>

          <p>All images or data your are uploading are saved in your personal cloud storage and available to all your slides.</p>
        </article>

        <article>
          <h3>Youtube</h3>

          <p>Embed easily Youtube videos. Play and pause through the remote control.</p>
        </article>

        <article>
          <h3>Unsplash and Giphy</h3>

          <p>Search and integrate freely stock photos and gifs.</p>
        </article>

        <article>
          <h3>Google fonts</h3>

          <p>Select a Google Font from a list we have preselected for your.</p>
        </article>

        <article>
          <h3>
            Custom fonts <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
          </h3>

          <p>Use your brand fonts.</p>
        </article>

        <article>
          <h3>Full screen edit</h3>

          <p>No more toggle to make a quick change, your slides are editable in full screen mode as well.</p>
        </article>

        <article>
          <h3>Transition</h3>

          <p>Make individual text element appear one after the others.</p>
        </article>

        <article>
          <h3>Math</h3>

          <p>Render math formula with Katex in your slides.</p>
        </article>

        <h2>Presenting</h2>

        <article>
          <h3>Remote control</h3>

          <p>Remote control your presentations, draw over your slides and set timer from your phone, tablet or any devices.</p>
        </article>

        <article>
          <h3>Speaker notes</h3>

          <p>Write notes for your slides. Cherry on top, Markdown is supported.</p>
        </article>

        <h2>More for enterprise</h2>

        <article>
          <h3>
            In housing <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
          </h3>

          <p>Integrate DeckDeckGo in your infrastructure and private network.</p>
        </article>

        <article>
          <h3>
            Custom domain <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
          </h3>

          <p>Publish your decks under with your org domain name.</p>
        </article>

        <article>
          <h3>
            Archive <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
          </h3>

          <p>List and find all your presentations in a single place.</p>
        </article>

        <article>
          <h3>
            Collaborate <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
          </h3>

          <p>Multiple concurrent users can edit the same presentation at the same time.</p>
        </article>

        <article>
          <h3>
            Library <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
          </h3>

          <p>Organize your company's media library and share it with your teams.</p>
        </article>

        <h2>Developers</h2>

        <article>
          <h3>Developer Kit</h3>

          <p>You rather like to code your presentation with HTML or Markdown? All public features used by our editor are available in a developer kit.</p>
        </article>

        <article>
          <h3>Open source</h3>

          <p>
            DeckDeckGo is open source. The code of our applications and components are available on&nbsp;
            <a href="http://github.com/deckgo/deckdeckgo">
              <ion-icon name="logo-github" area-label="Github"></ion-icon> Github
            </a>
          </p>
        </article>

        <p>
          <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon> Available for organizations. Get in touch for a tailored discussion about
          your needs and pricing.
        </p>
      </section>
    );
  }
}
