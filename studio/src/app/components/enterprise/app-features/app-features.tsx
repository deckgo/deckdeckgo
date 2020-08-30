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
        <div class="content">
          <h2>Main features</h2>

          <article>
            <h3>Edit anywhere</h3>

            <p>The editor is available anywhere, it is a Progressive Web App!</p>
          </article>

          <article>
            <h3>Showcase everywhere</h3>

            <p>Presentations work on computers, phones, tablets and more. They are published as standalone apps.</p>
          </article>

          <article>
            <h3>Privacy per default</h3>

            <p>Your presentations and content are private. You can choose to share them publicly anytime, but that's your choice.</p>
          </article>

          <article>
            <h3>Share standalone apps</h3>

            <p>Your presentation are published as standalone Progressive Web Apps. Share them by sending your colleagues and friends a link.</p>
          </article>

          <article>
            <h3>Push to GitHub</h3>

            <p>
              Get the source code of your apps pushed automatically to your GitHub <ion-icon name="logo-github"></ion-icon> repos.
            </p>
          </article>

          <article>
            <h3>
              Share privately <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
            </h3>

            <p>Create private links to prevent access to your published content.</p>
          </article>

          <article>
            <h3>
              Export to PDF <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
            </h3>

            <p>If you like PDFs more than apps.</p>
          </article>

          <article>
            <h3>Polls</h3>

            <p>Engage with your audience in real time. Get them involved during your presentations with their smartphones and show live results.</p>
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

            <p>Apply your design, colors, fonts, styles and any other options to your templates. Those will be used by all your collaborators.</p>
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

            <p>All data (text, images, etc) you upload are saved in your personal cloud storage and available to all slides.</p>
          </article>

          <article>
            <h3>YouTube</h3>

            <p>Embed YouTube videos easily. Play and pause through the remote control.</p>
          </article>

          <article>
            <h3>Unsplash and Tenor GIFs</h3>

            <p>Search and integrate stock photos and GIFs.</p>
          </article>

          <article>
            <h3>Google Fonts</h3>

            <p>Select a Google Font from a list we have preselected for you.</p>
          </article>

          <article>
            <h3>
              Custom fonts <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
            </h3>

            <p>Use your own brand fonts.</p>
          </article>

          <article>
            <h3>Full screen edit</h3>

            <p>No more switch to make a quick change, your slides are editable in full screen mode as well.</p>
          </article>

          <article>
            <h3>Transition</h3>

            <p>Make individual text elements appear one after the other.</p>
          </article>

          <article>
            <h3>Math</h3>

            <p>Render Math formulas with Katex in your slides.</p>
          </article>

          <article>
            <h3>Charts</h3>

            <p>Plot simple lines, area, bar, pie or donut charts.</p>
          </article>

          <h2>Presenting</h2>

          <article>
            <h3>Remote control</h3>

            <p>Control your presentations remotely, draw over your slides and set a timer from your phone or tablet.</p>
          </article>

          <article>
            <h3>Speaker notes</h3>

            <p>Write notes for your slides. Cherry on top, Markdown is supported.</p>
          </article>

          <h2>More for Enterprise</h2>

          <article>
            <h3>
              In housing <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
            </h3>

            <p>Integrate DeckDeckGo into your private infrastructure and networks.</p>
          </article>

          <article>
            <h3>
              Custom domain <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
            </h3>

            <p>Publish your decks under your own domain name.</p>
          </article>

          <article>
            <h3>
              Always up-to-date <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
            </h3>

            <p>Propagate your corporate design improvements to all your presentations.</p>
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

            <p>Multiple users can edit a presentation at the same time.</p>
          </article>

          <article>
            <h3>
              Library <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
            </h3>

            <p>Organize your company's media library and share it with your teams.</p>
          </article>

          <article>
            <h3>
              Support <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon>
            </h3>

            <p>Your inquiries have the highest priorities.</p>
          </article>

          <h2>Developers</h2>

          <article>
            <h3>Developer Kit</h3>

            <p>Would you rather prepare your presentation with HTML or Markdown? All public features used by our editor are available as a developer kit.</p>
          </article>

          <article>
            <h3>Open source</h3>

            <p>
              DeckDeckGo is open source. All code of our applications and components are available on&nbsp;
              <a href="http://github.com/deckgo/deckdeckgo" rel="noopener noreferrer">
                GitHub&nbsp;
                <ion-icon name="logo-github" aria-label="GitHub"></ion-icon>
              </a>
              .
            </p>

            <p>We encourage enterprise to adopt this approach but we do understand if you prefer to keep your corporate templates private.</p>
          </article>

          <p>
            <ion-icon name="business-outline" aria-label="Enterprise only"></ion-icon> Above features identified with an "enterprise" icon are available upon
            request for organizations.
          </p>
        </div>
      </section>
    );
  }
}
