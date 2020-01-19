import {Component, Element, h, State} from '@stencil/core';

import {isMobile} from '@deckdeckgo/utils';

@Component({
    tag: 'app-landing-deck',
    styleUrl: 'app-landing-deck.scss',
    shadow: false
})
export class AppLandingDeck {

    @Element() el: HTMLElement;

    @State()
    private deckIsBeginning: boolean = true;

    @State()
    private deckIsEnd: boolean = false;

    @State()
    private deckTransition: 'slide' | 'fade' = isMobile() ? 'fade' : 'slide';

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
        return <deckgo-deck embedded={true} transition={this.deckTransition}
                            onSlideNextDidChange={() => this.updateDeckPosition()}
                            onSlidePrevDidChange={() => this.updateDeckPosition()}>
            <deckgo-slide-title style={{'--background': 'var(--ion-color-primary)', '--color': 'white'}}>
                <h1 slot="title">Make more than presentations</h1>
                <div slot="content" style={{'margin-bottom': '48px'}}>
                    <h3>Create, present and share apps. Interact with your audience.</h3>

                    <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="light">
                        <ion-label style={{'text-transform': 'none'}}>Get started with DeckDeckGo</ion-label>
                    </ion-button>
                </div>

                {this.renderSlideBackground('start')}
            </deckgo-slide-title>

            <deckgo-slide-title>
                <h2 slot="title">Edit your slides anywhere. Display them everywhere.</h2>
                <div slot="content" style={{'display': 'flex', 'justify-content': 'center', 'align-items': 'center', 'max-width': '100%'}}>
                    <ion-icon lazy={true} class="ion-padding" src="/assets/img/landing/mobile-light.svg" style={{'font-size': '4rem'}}></ion-icon>
                    <ion-icon lazy={true} class="ion-padding" src="/assets/img/landing/tablet-light.svg" style={{'font-size': '6rem'}}></ion-icon>
                    <ion-icon lazy={true} class="ion-padding" src="/assets/img/landing/desktop-light.svg" style={{'font-size': '6.6rem'}}></ion-icon>
                    <ion-icon lazy={true} class="ion-padding" src="/assets/img/landing/projector.svg" style={{'font-size': '6.6rem'}}></ion-icon>
                </div>

                {this.renderSlideBackground('end')}
            </deckgo-slide-title>

            <deckgo-slide-author img-src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" img-alt="Predefined rich and responsive templates for a quick editing">
                <h2 slot="author" class="ion-text-center" style={{'margin-top': '64px'}}>
                    Use predefined rich and responsive templates for a quick editing.
                </h2>

                {this.renderSlideBackground('start')}
            </deckgo-slide-author>

            <deckgo-slide-title>
                <h2 slot="title">Interact with your audience with a live poll.</h2>
                <h3 slot="content">Interact with your presentation with a remote control.</h3>

                {this.renderSlideBackground('end')}
            </deckgo-slide-title>

            <deckgo-slide-title>
                <h2 slot="title">Search Unsplash and Tenor GIFs.</h2>
                <h3 slot="content">Integrate easily Youtube video.</h3>

                {this.renderSlideBackground('start', 'https://images.unsplash.com/photo-1516476892398-bdcab4c8dab8?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=1080&fit=max&ixid=eyJhcHBfaWQiOjcyMTQyfQ', 'Photo by Rodrigo Gonçalves on Unsplash')}
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

            <deckgo-slide-title style={{'--background': 'var(--ion-color-primary)', '--color': 'white'}}>
                <h1 slot="title">And many more features</h1>
                <div slot="content" style={{'margin-bottom': '48px'}}>
                    <h3>Create your slides with DeckDeckGo now.</h3>

                    <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="light">
                        <ion-label style={{'text-transform': 'none'}}>Start a presentation</ion-label>
                    </ion-button>
                </div>

                {this.renderSlideBackground('end')}
            </deckgo-slide-title>
        </deckgo-deck>
    }

    private renderSlideBackground(wave: 'start' | 'end', imgSrc?: string, imgAlt?: string) {
        return <div slot="background">
            {
                !this.deckIsEnd ? <button type="button" class="action next" onClick={() => this.prevNextSlide(true)}><ion-icon src="/assets/icons/ionicons/ios-arrow-forward.svg" aria-label="Next DeckDeckGo feature"></ion-icon></button> : undefined
            }

            {
                !this.deckIsBeginning ? <button type="button" class="action prev" onClick={() => this.prevNextSlide(false)}><ion-icon src="/assets/icons/ionicons/ios-arrow-back.svg" aria-label="Next DeckDeckGo feature"></ion-icon></button> : undefined
            }

            <img class="wave" src={`/assets/img/landing/wave-${wave}.svg`} role="presentation"/>

            {
                imgSrc && imgAlt ? <img class="background" data-src={imgSrc} alt={imgAlt}></img> : undefined
            }
        </div>
    }

}
