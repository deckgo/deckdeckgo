import {Component, Element, h, Listen} from '@stencil/core';

import {DeckdeckgoDocsUtils} from '../../../../utils/deckdeckgo-docs-utils';

@Component({
  tag: 'app-slides-concept',
  styleUrl: 'app-slides-concept.scss',
})
export class AppSlidesConcept {
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

  playPauseVideo(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const element: any = this.el.querySelector('deckgo-slide-video');

      if (!element) {
        resolve();
        return;
      }

      await element.toggle();

      resolve();
    });
  }

  render() {
    return [
      <app-navigation></app-navigation>,

      <ion-content class="ion-padding">
        <main>
          <h1 id="app-slides-concept-concept">Concept</h1>
          <p>
            <a href="https://deckdeckgo.com">DeckDeckGo</a> is a deck of slides where each slide is based on a template which has its own layout and behaviour.
            Their content could be edited and structured using <code>slots</code> and other attributes.
          </p>
          <p>
            The parent deck should be declared using the tag <code>&lt;deckgo-deck/&gt;</code> and each slide should be added as its children.
          </p>
          <deckgo-highlight-code language="javascript">
            <code slot="code">
              &lt;deckgo-deck&gt;{'\n'} &lt;deckgo-slide-title&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;The first slide&lt;&#47;h1&gt;{'\n'} &lt;p
              slot=&quot;content&quot;&gt;{'\n'} Hello World 🚀{'\n'} &lt;&#47;p&gt;{'\n'} &lt;&#47;deckgo-slide-title&gt;{'\n'}
              {'\n'} &lt;deckgo-slide-content&gt;{'\n'} &lt;h1 slot=&quot;title&quot;&gt;The second slide&lt;&#47;h1&gt;{'\n'} &lt;&#47;deckgo-slide-content&gt;
              {'\n'}&lt;&#47;deckgo-deck&gt;
            </code>
          </deckgo-highlight-code>
          <p>
            In the previous example, the presentation contains two slides. The first slide use the template <code>deckgo-slide-title</code> and the second slide
            use the template <code>deckgo-slide-content</code>.
          </p>
          <h1 id="app-slides-concept-installation">Installation</h1>
          <p>
            The core component of <a href="%60%3Cdeckgo-deck/%3E%60">DeckDeckGo</a> does not contain any slides, these have to be explicitly installed and
            imported. Doing so, only these, which you are actually using, are going to be bundled in your presentations for the best performances.
          </p>
          <blockquote>
            <p>If you are using the Starter Kit, per default, all our templates, these listed here behind, are pre-installed and pre-imported.</p>
          </blockquote>
          <h1 id="app-slides-concept-templates">Templates</h1>
          <p>
            <a href="https://deckdeckgo.com">DeckDeckGo</a> provide the following templates:
          </p>
          <ul>
            <li>
              Slide: <a href="/slides/title">Title</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-title>
                <h1 slot="title">slot="title"</h1>
                <p slot="content">slot="content"</p>
              </deckgo-slide-title>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/content">Content</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-content>
                <h1 slot="title">slot="title"</h1>
                <p slot="content">slot="content"</p>
              </deckgo-slide-content>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/split">Split</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-split>
                <h1 slot="title">slot="title"</h1>
                <p slot="start">slot="start"</p>
                <p slot="end">slot="end"</p>
              </deckgo-slide-split>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/gif">GIF</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-gif src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" alt="My gif" fullscreen={true}>
                <h1 slot="title">slot="title"</h1>
                <h1 slot="header" style={{fontSize: 'var(--font-size-h1)'}}>
                  slot="header"
                </h1>
                <h2 slot="footer" style={{fontSize: 'var(--font-size-normal)'}}>
                  slot="footer"
                </h2>
              </deckgo-slide-gif>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/bigimg">Big Image</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-big-img
                img-src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/slides/big-img/showcase/big-deckdeckgo-h.jpg"
                img-divisions="900;1500;2200"
                axis="x"
                reverse></deckgo-slide-big-img>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/chart">Chart</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-chart
                width={200}
                height={100}
                src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv">
                <h1 slot="title">slot="title"</h1>
              </deckgo-slide-chart>
              <deckgo-slide-chart
                width={200}
                height={100}
                type="line"
                y-axis-domain="extent"
                date-pattern="dd.MM.yyyy"
                src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
                <h1 slot="title">slot="title"</h1>
              </deckgo-slide-chart>
              <deckgo-slide-chart
                width={200}
                height={100}
                type="bar"
                src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
                style={{
                  '--deckgo-chart-fill-color-bar1': 'var(--ion-color-primary)',
                  '--deckgo-chart-fill-color-bar2': 'var(--ion-color-secondary)',
                  '--deckgo-chart-fill-color-bar3': 'var(--ion-color-tertiary)',
                }}>
                <h1 slot="title">slot="title"</h1>
              </deckgo-slide-chart>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/youtube">YouTube</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-youtube src="https://www.youtube.com/watch?v=oUOjJIfPIjw">
                <h1 slot="title">slot="title"</h1>
              </deckgo-slide-youtube>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/video">Video</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-video src="https://media.giphy.com/media/vv41HlvfogHAY/giphy.mp4">
                <h1 slot="title">A GIF as video</h1>
                <button slot="actions" onClick={() => this.playPauseVideo()}>
                  Play/pause
                </button>
              </deckgo-slide-video>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/code">Code</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-code src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/slides/code/src/components/slide/deckdeckgo-slide-code.tsx">
                <h1 slot="title">slot="title"</h1>
              </deckgo-slide-code>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/author">Author</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-author img-mode="circle" img-src="https://secure.meetupstatic.com/photos/member/9/c/4/2/member_272620002.jpeg">
                <h1 slot="title">slot="title"</h1>
                <div slot="author">slot="author"</div>
                <div slot="social-link" style={{fontSize: '0.5rem'}}>
                  <deckgo-social twitter="daviddalbusco">
                    <ion-icon aria-label="David on Twitter" slot="icon" name="logo-twitter"></ion-icon>
                  </deckgo-social>
                </div>
                <div slot="social-link" style={{fontSize: '0.5rem'}}>
                  <deckgo-social linkedin="david-dal-busco">
                    <ion-icon aria-label="David on LinkedIn" slot="icon" name="logo-linkedin"></ion-icon>
                  </deckgo-social>
                </div>
              </deckgo-slide-author>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/qrcode">QR Code</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-qrcode content="https://deckdeckgo.com">
                <h1 slot="title">slot="title"</h1>
                <p slot="content">slot="content"</p>
              </deckgo-slide-qrcode>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/countdown">Countdown</a>
            </li>
          </ul>
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

          <ul>
            <li>
              Slide: <a href="/slides/poll">Poll</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-poll poll-link="https://deckdeckgo.com/poll" socket-url="https://api.deckdeckgo.com" connectPollSocket={false}>
                <h1 slot="question">Do you like my presentation so far?</h1>
                <p slot="answer-1">It is super</p>
                <p slot="answer-2">Meh</p>
                <p slot="answer-3">I could'nt care less</p>
                <p slot="answer-4">Tell me why</p>
                <p slot="how-to">
                  Go to <a href="https://deckdeckgo.com/poll">deckdeckgo.com/poll</a> and use the code {0}
                </p>
                <p slot="awaiting-votes">Awaiting first votes</p>
                <p slot="answer-5">Ain't nothin' but a heartache</p>
              </deckgo-slide-poll>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/aspectratio">Aspect Ratio</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-aspect-ratio grid={true}>
                <h1 style={{position: 'absolute', top: '50%', left: '50%', transform: 'translate(-50%, -50%)', margin: '0'}}>Any elements</h1>
              </deckgo-slide-aspect-ratio>
            </deckgo-deck>
          </div>

          <ul>
            <li>
              Slide: <a href="/slides/playground">Playground</a>
            </li>
          </ul>
          <div class="container ion-margin">
            <deckgo-deck embedded={true}>
              <deckgo-slide-playground src="https://codepen.io/peterpeterparker/pen/dyGbOZm">
                <h1 slot="title">Playground</h1>
              </deckgo-slide-playground>
            </deckgo-deck>
          </div>
        </main>

        <app-footer></app-footer>
      </ion-content>,
    ];
  }
}
