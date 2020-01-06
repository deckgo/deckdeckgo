import {Component, Element, h, Host, State} from '@stencil/core';

import {debounce, isIOS} from '@deckdeckgo/utils';

@Component({
    tag: 'app-landing',
    styleUrl: 'app-landing.scss',
    shadow: false
})
export class AppLanding {

    @Element() el: HTMLElement;

    @State()
    private videoWidth: number | undefined = undefined;

    @State()
    private videoHeight: number | undefined =  undefined

    @State()
    private deckIsBeginning: boolean = true;

    @State()
    private deckIsEnd: boolean = false;

    async componentWillLoad() {
        await this.initVideoSize();

        this.initWindowResize();
    }

    async componentDidUnload() {
        this.removeWindowResize();
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

    private onWindowResize = async () => {
        await this.initVideoSize();

        const iframe: HTMLIFrameElement = this.el.querySelector('iframe');

        if (iframe) {
            iframe.width = '' + this.videoWidth;
            iframe.height = '' + this.videoHeight;
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

    private async updateDeckPosition() {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        this.deckIsBeginning = await (deck as any).isBeginning();
        this.deckIsEnd = await (deck as any).isEnd();
    }

    render() {
        return <Host>
            <section class="header">
                <deckgo-deck embedded={true}
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
                            <ion-icon class="ion-padding" src="/assets/img/landing/mobile-light.svg" style={{'font-size': '4rem'}}></ion-icon>
                            <ion-icon class="ion-padding" src="/assets/img/landing/tablet-light.svg" style={{'font-size': '6rem'}}></ion-icon>
                            <ion-icon class="ion-padding" src="/assets/img/landing/desktop-light.svg" style={{'font-size': '6.6rem'}}></ion-icon>
                            <ion-icon class="ion-padding" src="/assets/img/landing/projector.svg" style={{'font-size': '6.6rem'}}></ion-icon>
                        </div>

                        {this.renderSlideBackground('end')}
                    </deckgo-slide-title>

                    <deckgo-slide-author img-src="https://media.giphy.com/media/xUA7baWfTjfHGLZc3e/giphy.gif" img-alt="Predefined rich and responsive templates for a quick editing">
                        <h2 slot="author" class="ion-text-center" style={{'margin-top': '64px'}}>
                            Use predefined rich and responsive templates for a quick editing.
                        </h2>

                        {this.renderSlideBackground('start')}
                    </deckgo-slide-author>

                    <deckgo-slide-poll style={{'--deckgo-qrcode-color-fill': 'var(--ion-color-dark)', '--slide-padding-top': '48px', '--deckgo-chart-fill-color-1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)'}}>
                        <div slot="question">
                            <h2>Live interactive audience participation.</h2>
                            <p class="wider-devices">Engage your audience or class in real time. Involve them to contribute to your presentations with their smartphones and show the results live.</p>
                        </div>
                        <p slot="answer-1">Cool</p>
                        <p slot="answer-2">Awesome</p>
                        <p slot="how-to">Go to <a href="https://deckdeckgo.com/poll">deckdeckgo.com/poll</a> {'and use the code {0}'}</p>
                        <p slot="awaiting-votes">Awaiting your vote</p>

                        {this.renderSlideBackground('end')}
                    </deckgo-slide-poll>

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
                        <h1 slot="title">And many more features.</h1>
                        <div slot="content" style={{'margin-bottom': '48px'}}>
                            <h3>Start now. Create your slides with DeckDeckGo.</h3>

                            <ion-button class="ion-margin-top" shape="round" href="/editor" routerDirection="root" mode="md" color="light">
                                <ion-label style={{'text-transform': 'none'}}>Write a presentation</ion-label>
                            </ion-button>
                        </div>

                        {this.renderSlideBackground('end')}
                    </deckgo-slide-title>
                </deckgo-deck>
            </section>

            <div class="introducing">
                <main>
                    <section class="ion-padding">
                        {this.renderIntroducingVideo()}

                        <div>
                            <h2>DeckDeckGo is a web open source editor for presentations</h2>

                            <p>It works on any devices (desktop, mobile or tablets), without any prior installation, and even makes your content editable in full screen mode. Unlike other presentation software, your slides are published as online applications, making them the fastest way to be shared.</p>
                        </div>
                    </section>
                </main>
            </div>
        </Host>
    }

    private renderIntroducingVideo() {
        if (this.videoHeight === undefined || this.videoWidth === undefined) {
            return undefined;
        }

        return <iframe width={this.videoWidth} height={this.videoHeight} src="https://www.youtube.com/embed/Y97mEj9ZYmE" frameborder="0"></iframe>;
    }

    private renderSlideBackground(wave: 'start' | 'end') {
        return <div slot="background">
            {
                !this.deckIsEnd ? <button type="button" class="action next" onClick={() => this.prevNextSlide(true)}><ion-icon src="/assets/icons/ionicons/arrow-forward.svg" aria-label="Next DeckDeckGo feature"></ion-icon></button> : undefined
            }

            {
                !this.deckIsBeginning ? <button type="button" class="action prev" onClick={() => this.prevNextSlide(false)}><ion-icon src="/assets/icons/ionicons/arrow-back.svg" aria-label="Next DeckDeckGo feature"></ion-icon></button> : undefined
            }

            <img src={`/assets/img/landing/wave-${wave}.svg`}/>
        </div>
    }
}
