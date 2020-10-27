import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-edit-notes',
})
export class AppEditNotes {
  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-edit-notes-notes">Notes</h1>
          <p>
            Notes can be added to any slides. For such purpose, use the related slot <code>notes</code> to the particular slide you wish to comment.
          </p>
          <p>
            Your notes are going to be automatically <code>displayed</code> in the <a href="https://deckdeckgo.app">remote control</a>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;div
              slot=&quot;notes&quot;&gt;A note regarding this particular slide{'\n'}
              {'\n'}And another note on a new line about it too.&lt;&#47;div&gt;{'\n'} &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-edit-notes-publishing-notes">Publishing Notes</h2>
          <p>
            If you are using the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to make your notes accessible to anyone, you will need to
            mark them with the attribute <code>show</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My presentation title&lt;&#47;h1&gt;{'\n'} &lt;div
              slot=&quot;notes&quot; show&gt;A note displayed in the presentation within a modal accessible for anyone&lt;&#47;div&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-title&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-edit-notes-markdown">Markdown</h2>
          <p>
            The <a href="https://deckdeckgo.app">remote control</a> supports Markdown for your notes too.
          </p>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
