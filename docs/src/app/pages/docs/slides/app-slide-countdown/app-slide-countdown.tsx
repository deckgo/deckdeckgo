import {Component, Element, h} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

import {MenuService} from '../../../../services/menu/menu.service';

@Component({
  tag: 'app-slide-countdown',
  styleUrl: 'app-slides-countdown.scss'
})
export class AppSlideCountdown {

  @Element() el: HTMLElement;

  private menuService: MenuService;

  constructor() {
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

      <ion-content class="ion-padding">
        <main><h1 id="app-slide-countdown-slide-countdown">Slide: Countdown</h1>
<p>The &quot;Countdown&quot; slide displays a countdown until your presentation starts.</p>
<p>It could be handy, for example when you organize a meetup, to display a countdown until the event start.</p>
<h2 id="app-slide-countdown-table-of-contents">Table of contents</h2>
<ul>
<li><a href="#app-slide-countdown-layout">Layout</a></li>
<li><a href="#app-slide-countdown-usage">Usage</a><ul>
<li><a href="#app-slide-countdown-usage-1">Usage</a></li>
<li><a href="#app-slide-countdown-slots">Slots</a></li>
<li><a href="#app-slide-countdown-notes">Notes</a></li>
</ul>
</li>
<li><a href="#app-slide-countdown-attributes">Attributes</a><ul>
<li><a href="#app-slide-countdown-example-without-any-slots">Example without any slots</a></li>
</ul>
</li>
<li><a href="#app-slide-countdown-theming">Theming</a></li>
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

<h2 id="app-slide-countdown-usage">Usage</h2>
<p>The &quot;Countdown&quot; slide&#39;s Web Component could be integrated using the tag <code>&lt;deckgo-slide-countdown/&gt;</code>.</p>
<h3 id="app-slide-countdown-usage-1">Usage</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}    &lt;deckgo-slide-countdown until=&quot;2019-08-05T23:25:59.000+02:00&quot;&gt;{'\n'}        &lt;h1 slot=&quot;title&quot;&gt;My presentation starts at&lt;&#47;h1&gt;{'\n'}        &lt;p slot=&quot;days&quot;&gt;Days&lt;&#47;p&gt;{'\n'}        &lt;p slot=&quot;hours&quot;&gt;Hours&lt;&#47;p&gt;{'\n'}        &lt;p slot=&quot;minutes&quot;&gt;Minutes&lt;&#47;p&gt;{'\n'}        &lt;p slot=&quot;seconds&quot;&gt;Seconds&lt;&#47;p&gt;{'\n'}    &lt;&#47;deckgo-slide-countdown&gt;{'\n'}&lt;&#47;deckgo-deck&gt;  </code>
    </deckgo-highlight-code><h3 id="app-slide-countdown-slots">Slots</h3>
<p>The slots <code>title</code> as well as <code>days</code>, <code>hours</code>, <code>minutes</code> and <code>seconds</code> are optional.</p>
<h3 id="app-slide-countdown-notes">Notes</h3>
<p>Optionally a slot <code>notes</code> could be use to add some notes regarding the particular slide. These will be automatically <code>displayed</code> in the <a href="https://deckdeckgo.app">remote control</a>.</p>
<p>If you are using the <a href="https://deckdeckgo.com">DeckDeckGo</a> starter kit and wish to make your notes accessible to anyone, you would need to mark them with an attribute <code>show</code>.</p>
<h2 id="app-slide-countdown-attributes">Attributes</h2>
<p>The time until your presentation should be provided to render the countdown. This value could be either passed through attributes <code>days</code>, <code>hours</code>, <code>minutes</code> and <code>seconds</code> or as a particular date using <code>until</code>. </p>
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
<tbody><tr>
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
<td>If you would provide a background for the all deck and a specific one for this slide, set this option to <code>true</code></td>
</tr>
<tr>
<td>custom-actions</td>
<td>boolean</td>
<td>false</td>
<td>If you would provide actions for the all deck and a specific one for this slide, set this option to <code>true</code></td>
</tr>
</tbody></table>
<p>If you would provide a date using <code>until</code>, note that the format should be provided as a valid and parsable date. See <a href="https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date/parse">Date.parse()</a> for more information about the format.</p>
<h3 id="app-slide-countdown-example-without-any-slots">Example without any slots</h3>
<deckgo-highlight-code language="javascript">
      <code slot="code">&lt;deckgo-deck&gt;{'\n'}  &lt;deckgo-slide-qrcode hours=&quot;0&quot; minutes=&quot;10&quot; seconds=&quot;45&quot;&gt;{'\n'}  &lt;&#47;deckgo-slide-code&gt;{'\n'}&lt;&#47;deckgo-deck&gt;  </code>
    </deckgo-highlight-code><h2 id="app-slide-countdown-theming">Theming</h2>
<p>The following theming options will affect this component if set on its host or parent.</p>
<table>
<thead>
<tr>
<th>CSS4 variable</th>
<th>Default</th>
<th>Note</th>
</tr>
</thead>
<tbody><tr>
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
<td>--slide-countdown-digits-width</td>
<td>16em</td>
<td>The width of two displayed digit (Example: 16:00:00, 16em is the width of a digits 00 or 16) including spacing</td>
</tr>
<tr>
<td>--slide-countdown-digits-width</td>
<td>6em</td>
<td>The width of the (two) digits</td>
</tr>
<tr>
<td>--slide-countdown-digits-height</td>
<td></td>
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
<td>5em</td>
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
</tbody></table>
</main>

        <app-footer></app-footer>
      </ion-content>
    ];
  }
}
