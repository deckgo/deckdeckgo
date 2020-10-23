import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slide-countdown',
  styleUrl: 'app-slide-countdown.scss',
})
export class AppSlideCountdown {
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
          <h1 id="app-slide-countdown-slide-countdown">Slide: Countdown</h1>
          <p>The &quot;Countdown&quot; slide displays a countdown until your presentation starts.</p>
          <p>It could be handy, for example when you organize a meetup, to display a countdown until the event start.</p>
          <h2 id="app-slide-countdown-table-of-contents">Table of contents</h2>
          <ul>
            <li>
              <a href="#app-slide-countdown-layout">Layout</a>
            </li>
            <li>
              <a href="#app-slide-countdown-installation">Installation</a>
              <ul>
                <li>
                  <a href="#app-slide-countdown-from-a-cdn">From a CDN</a>
                </li>
                <li>
                  <a href="#app-slide-countdown-from-npm">From NPM</a>
                </li>
                <li>
                  <a href="#app-slide-countdown-framework-integration">Framework integration</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-countdown-usage">Usage</a>
              <ul>
                <li>
                  <a href="#app-slide-countdown-usage-1">Usage</a>
                </li>
                <li>
                  <a href="#app-slide-countdown-slots">Slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-countdown-attributes">Attributes</a>
              <ul>
                <li>
                  <a href="#app-slide-countdown-example-without-any-slots">Example without any slots</a>
                </li>
              </ul>
            </li>
            <li>
              <a href="#app-slide-countdown-theming">Theming</a>
            </li>
          </ul>
          <h2 id="app-slide-countdown-layout">Layout</h2>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-countdown hours={1} minutes={0} seconds={5}>
                <h1 slot="title">slot="title"</h1>
                <p slot="hours">slot="hours"</p>
                <p slot="minutes">slot="minutes"</p>
                <p slot="seconds">slot="seconds"</p>
              </deckgo-slide-countdown>
            </deckgo-deck>
          </div>

          <h2 id="app-slide-countdown-installation">Installation</h2>
          <p>This template could be added to your presentation using the following methods.</p>
          <blockquote>
            <p>
              If you are using our Starter Kit, no need to worry about this, this template is included, therefore you could skip the &quot;Installation&quot;
              chapter.
            </p>
          </blockquote>
          <h3 id="app-slide-countdown-from-a-cdn">From a CDN</h3>
          <p>
            It&#39;s recommended to use <a href="https://unpkg.com/">unpkg</a> if you want to use this template from a CDN. To do so, add the following include
            script in the main HTML file of your project:
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;script type=&quot;module&quot;
              src=&quot;https:&#47;&#47;unpkg.com&#47;@deckdeckgo&#47;slide-countdown@latest&#47;dist&#47;deckdeckgo-slide-countdown&#47;deckdeckgo-slide-countdown.esm.js&quot;&gt;&lt;&#47;script&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-countdown-from-npm">From NPM</h3>
          <p>
            To install this template in your project from <a href="https://www.npmjs.com/package/@deckdeckgo/slide-countdown">npm</a> run the following command:
          </p>
          <deckgo-highlight-code language="bash">
            <code slot="code">npm install @deckdeckgo&#47;slide-countdown</code>
          </deckgo-highlight-code>
          <h3 id="app-slide-countdown-framework-integration">Framework integration</h3>
          <p>
            The <a href="https://stenciljs.com/docs/overview">Stencil documentation</a> provide examples of framework integration for{' '}
            <a href="https://stenciljs.com/docs/angular">Angular</a>, <a href="https://stenciljs.com/docs/react">React</a>,{' '}
            <a href="https://stenciljs.com/docs/vue">Vue</a> and <a href="https://stenciljs.com/docs/ember">Ember</a>.
          </p>
          <p>
            That being said, commonly, you might either <code>import</code> or <code>load</code> it:
          </p>
          <h4 id="app-slide-countdown-import">Import</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">import &#039;@deckdeckgo&#47;slide-countdown&#039;;</code>
          </deckgo-highlight-code>
          <h4 id="app-slide-countdown-loader">Loader</h4>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              import &#123; defineCustomElements as deckDeckGoSlideElement &#125; from &#039;@deckdeckgo&#47;slide-countdown&#47;dist&#47;loader&#039;;{'\n'}
              deckDeckGoSlideElement();
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-countdown-usage">Usage</h2>
          <p>
            The &quot;Countdown&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-countdown/&gt;</code>.
          </p>
          <h3 id="app-slide-countdown-usage-1">Usage</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-countdown until=&quot;2019-08-05T23:25:59.000+02:00&quot;&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;My
              presentation starts at&lt;&#47;h1&gt;{'\n'} &lt;p slot=&quot;days&quot;&gt;Days&lt;&#47;p&gt;{'\n'} &lt;p
              slot=&quot;hours&quot;&gt;Hours&lt;&#47;p&gt;{'\n'} &lt;p slot=&quot;minutes&quot;&gt;Minutes&lt;&#47;p&gt;{'\n'} &lt;p
              slot=&quot;seconds&quot;&gt;Seconds&lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-slide-countdown&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h3 id="app-slide-countdown-slots">Slots</h3>
          <p>
            The slots <code>title</code> as well as <code>days</code>, <code>hours</code>, <code>minutes</code> and <code>seconds</code> are optional.
          </p>
          <h2 id="app-slide-countdown-attributes">Attributes</h2>
          <p>
            The time until your presentation should be provided to render the countdown. This value could be either passed through attributes <code>days</code>,{' '}
            <code>hours</code>, <code>minutes</code> and <code>seconds</code> or as a particular date using <code>until</code>.
          </p>
          <p>This components exposes the following attributes:</p>
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
                <td>days</td>
                <td>number</td>
                <td>0</td>
                <td>The amount of days before your presentations (max. 99 will be displayed)</td>
              </tr>
              <tr>
                <td>hours</td>
                <td>number</td>
                <td>0</td>
                <td>The amount of hours before your presentations (max. 23)</td>
              </tr>
              <tr>
                <td>minutes</td>
                <td>number</td>
                <td>0</td>
                <td>The amount of minutes before your presentations (max. 59)</td>
              </tr>
              <tr>
                <td>seconds</td>
                <td>number</td>
                <td>0</td>
                <td>The amount of seconds before your presentations (max. 59)</td>
              </tr>
              <tr>
                <td>until</td>
                <td>string</td>
                <td></td>
                <td>A specific date and time until when your presentation will start</td>
              </tr>
              <tr>
                <td>custom-background</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  If you would provide a background for the all deck and a specific one for this slide, set this option to <code>true</code>
                </td>
              </tr>
              <tr>
                <td>custom-actions</td>
                <td>boolean</td>
                <td>false</td>
                <td>
                  If you would provide actions for the all deck and a specific one for this slide, set this option to <code>true</code>
                </td>
              </tr>
            </tbody>
          </table>
          <p>
            If you would provide a date using <code>until</code>, note that the format should be provided as a valid and parsable date. See{' '}
            <a href="https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date/parse">Date.parse()</a> for more information about the
            format.
          </p>
          <h3 id="app-slide-countdown-example-without-any-slots">Example without any slots</h3>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-countdown hours=&quot;0&quot; minutes=&quot;10&quot; seconds=&quot;45&quot;&gt;{'\n'}{' '}
              &lt;&#47;deckgo-slide-countdown&gt;{'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <h2 id="app-slide-countdown-theming">Theming</h2>
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
                <td>--slide-countdown-container-padding-bottom</td>
                <td>64px</td>
                <td>The bottom padding of the displayed time container</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-max-width</td>
                <td>36em</td>
                <td>The max width of the container containing all digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-minmax-width</td>
                <td>12em</td>
                <td>The grid minmax value of one of the three columns of digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-width</td>
                <td>4em</td>
                <td>The width of one of the (two) digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-height</td>
                <td>4m</td>
                <td>The height of the (two) digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-background</td>
                <td></td>
                <td>The background color of the (two) digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-border-radius</td>
                <td></td>
                <td>The border-radius of the (two) digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-box-shadow</td>
                <td>0 3px 4px 0 rgba(0, 0, 0, .2), inset 2px 4px 0 0 rgba(255, 255, 255, .08)</td>
                <td>The box-shadow of the (two) digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digit-margin-right</td>
                <td>0.625em</td>
                <td>The space between two digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-font-size</td>
                <td>3em</td>
                <td>The border-radius of the (two) digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-font-weight</td>
                <td></td>
                <td>The font-weight of the (two) digits</td>
              </tr>
              <tr>
                <td>--slide-countdown-digits-color</td>
                <td></td>
                <td>The color of the (two) digits</td>
              </tr>
            </tbody>
          </table>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
