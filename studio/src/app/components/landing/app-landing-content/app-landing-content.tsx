import {Component, Element, h, Host, State} from '@stencil/core';

import {debounce, isIOS} from '@deckdeckgo/utils';

@Component({
    tag: 'app-landing-content',
    styleUrl: 'app-landing-content.scss',
    shadow: false
})
export class AppLandingContent {

    @Element() el: HTMLElement;

    @State()
    private videoWidth: number | undefined = undefined;

    @State()
    private videoHeight: number | undefined = undefined;

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
        return <Host>
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

                <deckgo-lazy-img svg-src={`/assets/img/landing/wave-audience.svg`} aria-label="Section introducing separator"></deckgo-lazy-img>
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

}
