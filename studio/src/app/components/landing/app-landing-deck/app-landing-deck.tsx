import {Component, Element, h, Listen, State} from '@stencil/core';

@Component({
  tag: 'app-landing-deck',
  styleUrl: 'app-landing-deck.scss',
  shadow: false,
})
export class AppLandingDeck {
  @Element() el: HTMLElement;

  @State()
  private deckIsBeginning: boolean = true;

  @State()
  private deckIsEnd: boolean = false;

  @Listen('ionRouteDidChange', {target: 'window'})
  async onRouteDidChange($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    if ($event.detail.to && $event.detail.to === '/' && $event.detail.from && $event.detail.to !== $event.detail.from) {
      await this.updateDeckSize();
    }
  }

  private updateDeckSize(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

      if (!deck) {
        resolve();
        return;
      }

      await (deck as any).initSlideSize();

      resolve();
    });
  }

  private async updateDeckPosition() {
    const deck: HTMLElement = this.el.querySelector('deckgo-deck');

    if (!deck) {
      return;
    }

    this.deckIsBeginning = await (deck as any).isBeginning();
    this.deckIsEnd = await (deck as any).isEnd();
  }

  private async prevNextSlide(next: boolean) {
    const deck: HTMLElement = this.el.querySelector('deckgo-deck');

    if (!deck) {
      return;
    }

    if (next) {
      await (deck as any).slideNext(false, true);
    } else {
      await (deck as any).slidePrev(false, true);
    }
  }

  render() {
    return (
      <deckgo-deck embedded={true} onSlideNextDidChange={() => this.updateDeckPosition()} onSlidePrevDidChange={() => this.updateDeckPosition()}>
        <deckgo-slide-split>
          <div slot="start">
            <h1>Make more than presentations</h1>
            <section style={{'font-weight': '300', 'margin-top': '16px', 'font-size': 'var(--font-size-h3)'}}>
              Create, present and share apps. Interact with your audience.
            </section>

            <ion-button class="ion-margin-top" style={{'margin-bottom': '32px'}} shape="round" href="/editor" routerDirection="root" mode="md" color="dark">
              <ion-label style={{'text-transform': 'none'}}>Get started with DeckDeckGo</ion-label>
            </ion-button>
          </div>

          <div slot="end">
            <img
              loading="lazy"
              src={`/assets/img/landing/illustrations/presentation.svg`}
              role="presentation"
              style={{width: 'calc(var(--slide-width) / 3)'}}
            />
          </div>

          {this.renderSlideBackground('start')}
        </deckgo-slide-split>

        <deckgo-slide-title>
          <h2 slot="title">Edit your slides anywhere. Display them everywhere.</h2>
          <div slot="content" style={{display: 'flex', 'justify-content': 'center', 'align-items': 'center', 'max-width': '100%'}}>
            <ion-icon lazy={true} class="ion-padding" src="/assets/img/landing/mobile-light.svg" style={{'font-size': '4rem'}}></ion-icon>
            <ion-icon lazy={true} class="ion-padding" src="/assets/img/landing/tablet-light.svg" style={{'font-size': '6rem'}}></ion-icon>
            <ion-icon lazy={true} class="ion-padding" src="/assets/img/landing/desktop-light.svg" style={{'font-size': '6.6rem'}}></ion-icon>
            <ion-icon lazy={true} class="ion-padding" src="/assets/img/landing/projector.svg" style={{'font-size': '6.6rem'}}></ion-icon>
          </div>

          {this.renderSlideBackground('end')}
        </deckgo-slide-title>

        <deckgo-slide-author
          img-src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif"
          img-alt="Predefined rich and responsive templates for a quick editing">
          <h2 slot="author" class="ion-text-center" style={{'margin-top': '64px'}}>
            Use predefined rich and responsive templates for a quick editing.
          </h2>

          {this.renderSlideBackground('start')}
        </deckgo-slide-author>

        <deckgo-slide-title>
          <h2 slot="title">Search Unsplash and Tenor GIFs.</h2>
          <h3 slot="content" style={{'font-weight': '300'}}>
            Easily integrate YouTube video.
          </h3>

          {this.renderSlideBackground(
            'end',
            'https://images.unsplash.com/photo-1516476892398-bdcab4c8dab8?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjcyMTQyfQ',
            'Photo by Rodrigo Gonçalves on Unsplash'
          )}
        </deckgo-slide-title>

        <deckgo-slide-title>
          <h2 slot="title">Showcase your code.</h2>
          <div slot="content" style={{'margin-bottom': '48px'}}>
            <deckgo-highlight-code class="ion-margin-top">
              <code slot="code">{`import React, { useState } from 'react';

function Example() {
  // Declare a new state variable, which we'll call "count"
  const [count, setCount] = useState(0);

  return (
    <div>
      <p>You clicked {count} times</p>
      <button onClick={() => setCount(count + 1)}>
        Click me
      </button>
    </div>
  );
}`}</code>
            </deckgo-highlight-code>
          </div>

          {this.renderSlideBackground('start')}
        </deckgo-slide-title>

        <deckgo-slide-split style={{'--slide-split-align': 'center'}}>
          <div slot="start">
            <h2>Interact with your presentation with a remote control.</h2>
          </div>

          <div slot="end">
            <img data-src={`/assets/img/landing/illustrations/progressive-app.svg`} role="presentation" style={{width: 'calc(var(--slide-width) / 2.5)'}} />
          </div>

          {this.renderSlideBackground('end')}
        </deckgo-slide-split>

        <deckgo-slide-title style={{'--color': 'white'}}>
          <h2
            slot="title"
            style={{
              background: 'rgba(var(--ion-color-light-rgb), 0.9)',
              color: 'var(--ion-color-light-contrast)',
              padding: '16px',
              'border-radius': '8px',
              'box-shadow': '8px 8px 16px rgba(0, 0, 0, 0.12)',
            }}>
            Interact with your audience with live polls.
          </h2>

          {this.renderSlideBackground(
            'start',
            'https://images.unsplash.com/photo-1501386761578-eac5c94b800a?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjcyMTQyfQ',
            'Photo by Nicholas Green on Unsplash'
          )}
        </deckgo-slide-title>

        <deckgo-slide-split style={{'--slide-split-align': 'center'}}>
          <div slot="start">
            <h2>Present and work offline.</h2>
          </div>

          <div slot="end">
            <img data-src={`/assets/img/landing/illustrations/travel.svg`} role="presentation" style={{width: 'calc(var(--slide-width) / 3)'}} />
          </div>

          {this.renderSlideBackground('end')}
        </deckgo-slide-split>

        <deckgo-slide-title>
          <h2 slot="title">And many more features</h2>
          <div slot="content" style={{'margin-bottom': '48px'}}>
            <h3 style={{'font-weight': '300'}}>Create your slides with DeckDeckGo now.</h3>

            <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="dark">
              <ion-label style={{'text-transform': 'none'}}>Start a presentation</ion-label>
            </ion-button>
          </div>

          {this.renderSlideBackground('start')}
        </deckgo-slide-title>
      </deckgo-deck>
    );
  }

  private renderSlideBackground(wave: 'start' | 'end', imgSrc?: string, imgAlt?: string) {
    return (
      <div slot="background">
        {!this.deckIsEnd ? (
          <button type="button" class="action next" onClick={() => this.prevNextSlide(true)} tabindex={-1}>
            <ion-icon name="chevron-forward-outline" aria-label="Next DeckDeckGo feature"></ion-icon>
          </button>
        ) : undefined}

        {!this.deckIsBeginning ? (
          <button type="button" class="action prev" onClick={() => this.prevNextSlide(false)} tabindex={-1}>
            <ion-icon name="chevron-back-outline" aria-label="Next DeckDeckGo feature"></ion-icon>
          </button>
        ) : undefined}

        <img class="wave" src={`/assets/img/landing/wave-${wave}.svg`} role="presentation" />

        {imgSrc && imgAlt ? <img class="background" data-src={imgSrc} alt={imgAlt}></img> : undefined}
      </div>
    );
  }
}
