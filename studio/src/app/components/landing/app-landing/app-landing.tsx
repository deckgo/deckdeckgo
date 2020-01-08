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
    private videoHeight: number | undefined =  undefined;

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

        const iframes: NodeListOf<HTMLIFrameElement> = this.el.querySelectorAll('iframe');

        if (iframes && iframes.length > 0) {
            Array.from(iframes).forEach((iframe: HTMLIFrameElement) => {
                iframe.width = '' + this.videoWidth;
                iframe.height = '' + this.videoHeight;
            });
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

    private loadPoll(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const slidePoll: HTMLElement = this.el.querySelector('deckgo-slide-poll.showcase');

            if (!slidePoll) {
                resolve();
                return;
            }

            await (slidePoll as any).lazyLoadContent();

            slidePoll.addEventListener('pollUpdated', debounce(async () => {
                await (slidePoll as any).resizeContent();
            }), {once: true});

            resolve();
        });
    }

    render() {

        // TODO add deep linking with QR code
        // TODO add footer

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

                    <deckgo-slide-title>
                        <h2 slot="title">Interact with your audience with a <strong>live poll</strong>.</h2>
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
                        {this.renderVideo('https://www.youtube.com/embed/Y97mEj9ZYmE')}

                        <div>
                            <h2>DeckDeckGo is a web open source editor for presentations</h2>

                            <p>It works on any devices (desktop, mobile or tablets), without any prior installation, and even makes your content editable in full screen mode. Unlike other presentation software, your slides are published as online applications, making them the fastest way to be shared.</p>
                        </div>
                    </section>
                </main>

                <deckgo-lazy-img svg-src={`/assets/img/landing/wave-introducing.svg`} aria-label="Section introducing separator"></deckgo-lazy-img>
            </div>

            <div class="audience">
                <main>
                    <section class="ion-padding">
                        <div>
                            <h2>Live interactive audience participation</h2>

                            <p>Engage your audience or class in real time. Involve them to contribute to your presentations with their smartphones and show the results live.</p>
                        </div>

                        {this.renderPollDemo()}
                    </section>
                </main>

                <deckgo-lazy-img svg-src={`/assets/img/landing/wave-remote.svg`} aria-label="Section introducing separator"></deckgo-lazy-img>
            </div>

            <div class="remote">
                <main>
                    <section class="ion-padding">
                        {this.renderVideo('https://www.youtube.com/embed/PnSNT5WpauE')}

                        <div>
                            <h2>Interact with your presentation</h2>

                            <p>Remote control your deck and access your speaker notes with our “remote control” application. No special hardware or configuration needed, it works on every devices and even synchronize the content of your slides.</p>
                        </div>
                    </section>
                </main>
            </div>
        </Host>
    }

    private renderVideo(url: string) {
        if (this.videoHeight === undefined || this.videoWidth === undefined) {
            return undefined;
        }

        return <iframe width={this.videoWidth} height={this.videoHeight} src={url} frameborder="0"></iframe>;
    }

    private renderPollDemo() {
        return <div class="deck">
            <deckgo-slide-poll onSlideDidLoad={async () => await this.loadPoll()} class="showcase" style={{'--deckgo-qrcode-color-fill': 'var(--ion-color-dark)', '--deckgo-chart-fill-color-1': 'var(--ion-color-primary)', '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)'}}>
                <p slot="question">Interact with your audience</p>
                <p slot="answer-1">Cool</p>
                <p slot="answer-2">Awesome</p>
                <p slot="how-to">Go to <a href="https://deckdeckgo.com/poll">deckdeckgo.com/poll</a> {'and use the code {0}'}</p>
                <p slot="awaiting-votes" style={{'font-size': 'var(--font-size-normal)'}}>Awaiting your vote</p>
            </deckgo-slide-poll>
        </div>
    }

    private renderSlideBackground(wave: 'start' | 'end', imgSrc?: string, imgAlt?: string) {
        return <div slot="background">
            {
                !this.deckIsEnd ? <button type="button" class="action next" onClick={() => this.prevNextSlide(true)}><ion-icon src="/assets/icons/ionicons/arrow-forward.svg" aria-label="Next DeckDeckGo feature"></ion-icon></button> : undefined
            }

            {
                !this.deckIsBeginning ? <button type="button" class="action prev" onClick={() => this.prevNextSlide(false)}><ion-icon src="/assets/icons/ionicons/arrow-back.svg" aria-label="Next DeckDeckGo feature"></ion-icon></button> : undefined
            }

            <img class="wave" src={`/assets/img/landing/wave-${wave}.svg`}/>

            {
                imgSrc && imgAlt ? <deckgo-lazy-img img-src={imgSrc} img-alt={imgAlt}></deckgo-lazy-img> : undefined
            }
        </div>
    }
}
