import {Component, Element, h, Host, State} from '@stencil/core';

import {debounce, isIOS} from '@deckdeckgo/utils';

@Component({
  tag: 'app-landing-content',
  styleUrl: 'app-landing-content.scss',
  shadow: false,
})
export class AppLandingContent {
  @Element() el: HTMLElement;

  @State()
  private videoWidth: number | undefined = undefined;

  @State()
  private videoHeight: number | undefined = undefined;

  private videoObserver: IntersectionObserver;

  private pollObserver: IntersectionObserver;

  async componentWillLoad() {
    await this.initVideoSize();

    this.initWindowResize();
  }

  async componentDidLoad() {
    if (window && 'IntersectionObserver' in window) {
      await this.deferVideoIntersectionObserverLoad();
      await this.deferPollIntersectionObserverLoad();
    } else {
      await this.unfortunatelyLoadVideoNow();
    }
  }

  async disconnectedCallback() {
    this.removeWindowResize();

    if (this.videoObserver) {
      this.videoObserver.disconnect();
    }

    if (this.pollObserver) {
      this.pollObserver.disconnect();
    }
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onWindowResize));
    }
  }

  private removeWindowResize() {
    if (window) {
      window.removeEventListener('resize', debounce(this.onWindowResize));
    }
  }

  private deferVideoIntersectionObserverLoad(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.videoObserver = new IntersectionObserver(this.onVideoIntersection, {
        rootMargin: '100px 0px',
        threshold: 0.25,
      });

      const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('div.video');

      if (elements) {
        Array.from(elements).forEach((element: HTMLElement) => {
          this.videoObserver.observe(element);
        });
      }

      resolve();
    });
  }

  private deferPollIntersectionObserverLoad(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.pollObserver = new IntersectionObserver(this.onPollIntersection, {
        rootMargin: '100px 0px',
        threshold: 0.25,
      });

      const element: HTMLElement = this.el.querySelector('deckgo-slide-poll');

      if (element) {
        this.pollObserver.observe(element);
      }

      resolve();
    });
  }

  private onVideoIntersection = async (entries: IntersectionObserverEntry[]) => {
    if (!entries || entries.length <= 0) {
      return;
    }

    await this.handleVideoIntersection(entries);
  };

  private onPollIntersection = async (entries: IntersectionObserverEntry[]) => {
    if (!entries || entries.length <= 0) {
      return;
    }

    await this.handlePollIntersection(entries[0]);
  };

  private handleVideoIntersection(entries: IntersectionObserverEntry[]): Promise<void> {
    return new Promise<void>(async (resolve) => {
      for (const entry of entries) {
        if (entry.isIntersecting) {
          if (this.videoObserver && entry.target) {
            await (entry.target.firstChild as any).lazyLoadContent();
            this.videoObserver.unobserve(entry.target);
          }
        }
      }

      resolve();
    });
  }

  private handlePollIntersection(entry: IntersectionObserverEntry): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (entry.isIntersecting) {
        await this.loadPoll();
        this.pollObserver.unobserve(entry.target);
      }

      resolve();
    });
  }

  private unfortunatelyLoadVideoNow(): Promise<void> {
    return new Promise<void>((resolve) => {
      const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-youtube');

      if (elements && elements.length > 0) {
        Array.from(elements).forEach(async (element: HTMLElement) => {
          await (element as any).lazyLoadContent();
        });
      }

      resolve();
    });
  }

  private onWindowResize = async () => {
    await this.initVideoSize();

    const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-youtube');

    if (elements && elements.length > 0) {
      for (const element of Array.from(elements)) {
        await (element as any).updateIFrame(this.videoWidth, this.videoHeight);
      }
    }
  };

  private initVideoSize(): Promise<void> {
    return new Promise<void>((resolve) => {
      const windowWidth: number = isIOS() ? screen.width : window.innerWidth;
      const maxWidth: number = (windowWidth > 1400 ? 1400 : windowWidth) - 64;

      this.videoWidth = windowWidth <= 768 ? windowWidth - 32 : maxWidth / 2;
      this.videoHeight = (this.videoWidth * 9) / 16;

      resolve();
    });
  }

  private async loadPollWithoutIntersectionObserver() {
    if (window && 'IntersectionObserver' in window) {
      return;
    }

    await this.loadPoll();
  }

  private loadPoll(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slidePoll: HTMLElement = this.el.querySelector('deckgo-slide-poll.showcase');

      if (!slidePoll) {
        resolve();
        return;
      }

      await (slidePoll as any).lazyLoadContent();

      slidePoll.addEventListener(
        'pollUpdated',
        debounce(async () => {
          await (slidePoll as any).resizeContent();
        }),
        {once: true}
      );

      resolve();
    });
  }

  render() {
    return (
      <Host>
        <div class="introducing">
          <main>
            <section class="ion-padding">
              {this.renderVideo('https://www.youtube.com/embed/Y97mEj9ZYmE', 'Demo of the editor')}

              <div>
                <h2>DeckDeckGo is a web open source editor for presentations</h2>

                <p>
                  It works on any devices (desktop, mobile or tablets), without any prior installation, and even makes your content editable in full screen
                  mode. Unlike other presentation software, your slides are published as online applications, making them the fastest way to be shared.
                </p>
              </div>
            </section>
          </main>

          <img src={`/assets/img/landing/wave-introducing.svg`} role="presentation" loading="lazy" class="wave-section" />
        </div>

        <div class="audience">
          <main>
            <section class="ion-padding">
              <div>
                <h2>Live interactive audience participation</h2>

                <p>
                  Engage your audience or class in real time. Involve them to contribute to your presentations with their smartphones and show the results live.
                </p>
              </div>

              {this.renderPollDemo()}
            </section>
          </main>

          <img src={`/assets/img/landing/wave-audience.svg`} role="presentation" loading="lazy" class="wave-section" />
        </div>

        <div class="remote">
          <main>
            <section class="ion-padding">
              {this.renderVideo('https://www.youtube.com/embed/PnSNT5WpauE', 'Interact with your presentation')}

              <div>
                <h2>Interact with your presentation</h2>

                <p>
                  Remote control your deck and access your speaker notes with our “remote control” application. No special hardware or configuration needed, it
                  works on every devices and even synchronize the content of your slides.
                </p>
              </div>
            </section>
          </main>
        </div>
      </Host>
    );
  }

  private renderVideo(url: string, alt: string) {
    return (
      <div class="video" style={{width: `${this.videoWidth}px`, height: `${this.videoHeight}px`}}>
        <deckgo-youtube width={this.videoWidth} height={this.videoHeight} src={url} frameTitle={alt}></deckgo-youtube>
      </div>
    );
  }

  private renderPollDemo() {
    return (
      <div class="deck">
        <deckgo-slide-poll
          onSlideDidLoad={async () => await this.loadPollWithoutIntersectionObserver()}
          class="showcase"
          style={{
            '--deckgo-qrcode-color-fill': '#222428',
            '--deckgo-chart-fill-color-1': 'var(--ion-color-primary)',
            '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)',
          }}>
          <p slot="question">Interact with your audience</p>
          <p slot="answer-1">Cool</p>
          <p slot="answer-2">Awesome</p>
          <p slot="how-to">
            Go to <a href="https://deckdeckgo.com/poll">deckdeckgo.com/poll</a> {'and use the code {0}'}
          </p>
          <p slot="awaiting-votes" style={{'font-size': 'var(--font-size-normal)'}}>
            Awaiting your vote
          </p>
        </deckgo-slide-poll>
      </div>
    );
  }
}
