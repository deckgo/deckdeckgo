import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-edit-default'
})
export class AppEditDefault {

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main><h1 id="app-edit-default-html">HTML</h1>
<p>To edit your <a href="https://deckdeckgo.com">DeckDeckGo</a> presentation you could either use <strong>HTML</strong> or <strong>Markdown</strong>.</p>
<p>The default language is <strong>HTML</strong>. It&#39;s also the recommended language as it offers you more flexibility specially in terms of styling.</p>
<p>If you have chosen to edit your presentation using HTML, begin by editing the <code>index.html</code> of your project as suggested in the chapter <a href="/slides/concept">Concept</a>.</p>
<p>Whereas if you prefer to edit your talk using <strong>Markdown</strong>, have a look at <a href="/edit/markdown">the next chapter</a>.</p>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
