import {Component, Element} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-publishing'
})
export class AppPublishing {

  @Element() el: HTMLElement;

  constructor(private menuService: MenuService) {
    this.menuService = MenuService.getInstance();
  }

  async componentWillLoad() {
    this.menuService.enable();
  }

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content padding>
        <main><h1 id="app-publishing-publishing">Publishing</h1>
<p><a href="https://deckdeckgo.com">DeckDeckGo</a> bundles your presentation as a Progressive Web App which could be hosted on any Web Server or hosting solution.</p>
<blockquote>
<p>Not sure what PWAs are? Check out Ionic&#39;s <a href="https://ionicframework.com/pwa">PWA Overview</a> for more info.</p>
</blockquote>
<h2 id="app-publishing-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-publishing-seo-friendly">SEO friendly</a></li>
<li><a href="#app-publishing-before-production">Before deploy</a></li>
<li><a href="#app-publishing-production-build">Production build</a></li>
</ul>
<h2 id="app-publishing-seo-friendly">SEO friendly</h2>
<p>It is worth to notice that <a href="https://deckdeckgo.com">DeckDeckGo</a> is, respectively your slides build with are, SEO friendly. </p>
<p>Therefore you do <strong>not</strong> need to implement a complex server-side rendering (SSR) hosting solution. </p>
<h2 id="app-publishing-before-production">Before production</h2>
<p>Before your final build and most important before deploying online your deck, don&#39;t forget to edit the information about your presentation in the following files:</p>
<ol>
<li><p>Edit the meta tags in the <code>&lt;head/&gt;</code> of your <code>src/index.html</code> file</p>
</li>
<li><p>Generate your icons and replace the respective files in the <code>assets</code> folder. For that purpose I suggest you to use the real great tool <a href="https://realfavicongenerator.net">RealFaviconGenerator</a></p>
</li>
<li><p>Update the information in the <code>manifest.json</code> file</p>
</li>
</ol>
<h2 id="app-publishing-production-build">Production build</h2>
<p>When you are ready for your talk or ready to publish online your slides, run the following command in a terminal to bundle your presentation for production:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm run build</code>
    </deckgo-highlight-code><p>If you do not wish to remove your notes from your presentation, run the build command with the attributes <code>--notes</code>:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm run build -- --notes</code>
    </deckgo-highlight-code><p>If you wish to run your presentation locally afterwards without rebuilding everything, you could run the following command to start only the dev server:</p>
<deckgo-highlight-code language="bash">
      <code slot="code">npm run dev</code>
    </deckgo-highlight-code></main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
