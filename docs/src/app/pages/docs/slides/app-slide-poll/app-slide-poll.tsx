import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-poll',
  styleUrl: 'app-slide-poll.scss',
})
export class AppSlidePoll {
  @Element() el: HTMLElement;

  async componentDidLoad() {
    await DeckdeckgoDocsUtils.reloadCode(this.el);
  }

  @Listen('slidesDidLoad')
  async onSlidesDidLoad($event: CustomEvent) {
    if ($event) {
      await DeckdeckgoDocsUtils.initSlideSize($event.target as HTMLElement);
    }
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-slide-poll-slide-poll">Slide: Poll</h1>
          <p>Engage your audience or class in real time. Involve them to contribute to your presentations with their smartphones and show the results live.</p>
          <p>Add a slide &quot;Poll&quot; to your presentation.</p>
          <h2 id="app-slide-poll-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-poll-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-poll-nota-bene">Nota bene</a>
            </li>
            <li>
              <a href="#app-slide-poll-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-poll-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-poll-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-poll-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-poll-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-poll-slots">Slots</a>
                </li>
                <li>
                  <a href="#app-slide-poll-youtube-component">YouTube component</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-poll-attributes">Attributes</a>
            </li>
            <li>
              <a href="#app-slide-poll-theming">Theming</a>
            </li>
            <li>
              <a href="#app-slide-poll-methods">Methods</a>
              <ul>
                <li>
                  <a href="#app-slide-poll-play-the-video">Play the video</a>
                </li>
                <li>
                  <a href="#app-slide-poll-pause-the-video">Pause the video</a>
                </li>
                <li>
                  <a href="#app-slide-poll-toggle-the-video">Toggle the video</a>
                </li>
                <li>
                  <a href="#app-slide-poll-get-the-video">Get the video</a>
                </li>
              </ul>
            </li>
          </ul>
          <h2 id="app-slide-poll-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-poll poll-link="https://app.deckdeckgo.com/poll" socket-url="https://api.deckdeckgo.com">
                <h1 slot="question">Do you like my presentation so far?</h1>
                <p slot="answer-1">It is super</p>
                <p slot="answer-2">Meh</p>
                <p slot="answer-3">I could'nt care less</p>
                <p slot="answer-4">Tell me why</p>
                <p slot="how-to">
                  Go to <a href="https://app.deckdeckgo.com/poll">app.deckdeckgo.com/poll</a> and use the code {0}
                </p>
                <p slot="awaiting-votes">Awaiting first votes</p>
                <p slot="answer-5">Ain't nothin' but a heartache</p>
              </deckgo-slide-poll>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-poll-nota-bene">Nota bene</h2>
          <p>
            This template does <strong>not</strong> currently save the results of the voting. Each time you will refresh or launch your presentation, the poll
            start again.
          </p>
          <p>
            If you would have this requirement, let us now with a new <a href="https://github.com/deckgo/deckdeckgo/issues">feature request</a> in our GitHub
            issue tracker.
          </p>
          <h2 id="app-slide-poll-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-poll-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> to use this template from a CDN. To do so, add the following include script in
            the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-poll@latest&#47;dist&#47;deckdeckgo-slide-poll&#47;deckdeckgo-slide-poll.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-poll-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-poll">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-poll</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-poll-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-poll-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-poll&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-poll-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-poll&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-poll-usage">Usage</h2>
          <p>
            The &quot;Poll&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-poll/&gt;</code>.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-slide-poll poll-link=&quot;https:&#47;&#47;deckdeckgo.com&quot; socket-url=&quot;https:&#47;&#47;api.deckdeckgo.com&quot;&gt;{'\n'}{' '}
              &lt;h1 slot=&quot;question&quot;&gt;Do you like my presentation so far?&lt;&#47;h1&gt;{'\n'} &lt;p slot=&quot;answer-1&quot;&gt;It is
              super&lt;&#47;p&gt;{'\n'} &lt;p slot=&quot;answer-2&quot;&gt;Meh&lt;&#47;p&gt;{'\n'} &lt;p slot=&quot;answer-3&quot;&gt;I could&#039;nt care
              less&lt;&#47;p&gt;{'\n'} &lt;p slot=&quot;answer-4&quot;&gt;Tell me why&lt;&#47;p&gt;{'\n'} &lt;p slot=&quot;answer-5&quot;&gt;Ain&#039;t
              nothin&#039; but a heartache&lt;&#47;p&gt;{'\n'} &lt;p slot=&quot;how-to&quot;&gt;Go to &lt;a
              href=&quot;https:&#47;&#47;deckdeckgo.com&#47;poll&quot;&gt;deckdeckgo.com&#47;poll&lt;&#47;a&gt; and use the code &#123;0&#125;&lt;&#47;p&gt;
              {'\n'} &lt;p slot=&quot;awaiting-votes&quot;&gt;Awaiting first votes&lt;&#47;p&gt;{'\n'}&lt;&#47;deckgo-slide-poll&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-poll-slots">Slots</h3>
          <p>
            The slots <code>question</code> and at least one <code>answer</code> should be provided. Answer slots have to be provided as <code>answer-x</code>{' '}
            where <code>x</code> is a number bigger than 0.
          </p>
          <p>
            The slot <code>how-to</code> and <code>awaiting-votes</code> are optional, still, it&#39;s probably for best of your audience to provide these.
          </p>
          <p>
            Note also that if you provide a string <code>{0}</code> in the content of your slot <code>how-to</code>, the information will be automatically
            converted to the real key of your poll (the key your audience could use to reach it and vote).
          </p>
          <h2 id="app-slide-poll-attributes">Attributes</h2>
          <p>This component offers the following options which could be set using attributes:</p>
          <table>
            <thead>
              <tr>
                <th>Attribute</th>
                <th>Type</th>
                <th>Default</th>
                <th>Description</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>socketUrl</td>
                <td>string</td>
                <td>
                  <code>https://api.deckdeckgo.com</code>
                </td>
                <td>The url of the socket (server) where the poll (chat room) is going to be created.</td>
              </tr>
              <tr>
                <td>socketPath</td>
                <td>string</td>
                <td>
                  <code>/poll</code>
                </td>
                <td>The path to reach the socket server</td>
              </tr>
              <tr>
                <td>connectPollSocket</td>
                <td>boolean</td>
                <td>
                  <code>true</code>
                </td>
                <td>In case you would not like that the template try to reach the socket server</td>
              </tr>
              <tr>
                <td>pollLink</td>
                <td>string</td>
                <td>
                  <code>https://app.deckdeckgo.com/poll</code>
                </td>
                <td>
                  The url which leads to the voting application respectively where your audience will be available to make their voice heard aka where they will
                  be able to vote.
                </td>
              </tr>
              <tr>
                <td>pollKey</td>
                <td>string</td>
                <td></td>
                <td>Per default the template will always try to create a new poll but if you set this value, it will try to retrieve an existing poll</td>
              </tr>
            </tbody>
          </table>
          <h2 id="app-slide-poll-theming">Theming</h2>
          <p>The following theming options will affect this component if set on its host or parent.</p>
          <table>
            <thead>
              <tr>
                <th>CSS4 variable</th>
                <th>Default</th>
                <th>Note</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>--background</td>
                <td></td>
                <td></td>
              </tr>
              <tr>
                <td>--color</td>
                <td></td>
                <td></td>
              </tr>
              <tr>
                <td>--slide-padding-top</td>
                <td>16px</td>
                <td>Padding top of the all slide</td>
              </tr>
              <tr>
                <td>--slide-padding-end</td>
                <td>32px</td>
                <td>Padding right of the all slide</td>
              </tr>
              <tr>
                <td>--slide-padding-bottom</td>
                <td>16px</td>
                <td>Padding bottom of the all slide</td>
              </tr>
              <tr>
                <td>--slide-padding-start</td>
                <td>32px</td>
                <td>Padding left of the all slide</td>
              </tr>
              <tr>
                <td>--slide-poll-grid-column-gap</td>
                <td>32px</td>
                <td>The column gap between the QR code and the chart</td>
              </tr>
              <tr>
                <td>--slide-poll-justify-content</td>
                <td>center</td>
                <td>The QR code column content justify position</td>
              </tr>
              <tr>
                <td>--slide-poll-align-items</td>
                <td>center</td>
                <td>The QR code column content items alignment</td>
              </tr>
              <tr>
                <td>--slide-poll-text-align</td>
                <td>center</td>
                <td>The QR code column text alignment</td>
              </tr>
              <tr>
                <td>--slide-poll-background</td>
                <td></td>
                <td>The background behind the QR code component</td>
              </tr>
              <tr>
                <td>--slide-poll-how-to-max-width</td>
                <td>calc(100% - 64px)</td>
                <td>The maximal width of the &quot;how-to&quot; slot</td>
              </tr>
              <tr>
                <td>--slide-poll-how-to-font-size</td>
                <td>0.8em</td>
                <td>The font-size of the text of thee slot &quot;how-to&quot;</td>
              </tr>
              <tr>
                <td>--slide-poll-awaiting-votes-z-index</td>
                <td>1</td>
                <td>The z-index of the &quot;awaiting-votes&quot; slot</td>
              </tr>
              <tr>
                <td>--slide-poll-awaiting-votes-background</td>
                <td>rgba(255,255,255,0.9)</td>
                <td>The background of the &quot;awaiting-votes&quot; slot</td>
              </tr>
              <tr>
                <td>--slide-poll-awaiting-votes-border-radius</td>
                <td>8px</td>
                <td>The border-radios of the &quot;awaiting-votes&quot; slot</td>
              </tr>
              <tr>
                <td>--slide-poll-awaiting-votes-padding</td>
                <td>8px</td>
                <td>The padding of the &quot;awaiting-votes&quot; slot</td>
              </tr>
            </tbody>
          </table>
          <p>
            Moreover, this component is using the Web Components &quot;QR Code&quot; and &quot;Bar chart&quot;. Their respective CSS4 variables could be applied
            too.
          </p>
          <h2 id="app-slide-poll-methods">Methods</h2>
          <p>The slide &quot;Poll&quot; offers some extra methods.</p>
          <h3 id="app-slide-poll-update">Update</h3>
          <p>Update not in progress, update the answers and chart of the poll.</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">const slide = deck.getElementsByTagName(&#039;deckgo-slide-poll&#039;);{'\n'}await slide.update();</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-poll-is-answered">Is answered</h3>
          <p>Test if the poll has been at least answered once by a member of your audience.</p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              const slide = deck.getElementsByTagName(&#039;deckgo-slide-poll&#039;);{'\n'}await slide.isAnswered(); &#47;&#47; resolve a boolean
            </code>
          </deckgo-highlight-code>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
