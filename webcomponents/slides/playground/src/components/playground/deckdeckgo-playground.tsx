import {Component, Element, Method, Host, Prop, h, Watch, State} from '@stencil/core';

import {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';

import {formatPlaygroundSrc} from '../utils/deckdeckgo-playground.utils';

import {DeckdeckgoPlaygroundTheme} from '../../declarations/deckdeckgo-playground-theme';

@Component({
  tag: 'deckgo-playground',
  styleUrl: 'deckdeckgo-playground.scss',
  shadow: true,
})
export class DeckdeckgoPlayground implements DeckdeckgoComponent {
  @Element() el: HTMLElement;

  @Prop() src: string;

  @Prop() width: number;
  @Prop() height: number;

  @Prop() frameTitle: string;

  @Prop() allowFullscreen: boolean = true;

  @Prop() instant: boolean = false;

  @Prop() theme: DeckdeckgoPlaygroundTheme = DeckdeckgoPlaygroundTheme.DEFAULT;

  @State()
  private loading: boolean = false;

  @State()
  private loaded: boolean = false;

  @State()
  private formattedSrc: string | undefined = undefined;

  private iframe!: HTMLIFrameElement;

  async componentDidLoad() {
    if (this.instant) {
      await this.lazyLoadContent();
    }
  }

  @Method()
  async updateIFrame(width: number, height: number) {
    const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');

    if (iframe) {
      iframe.width = '' + width;
      iframe.height = '' + height;
    }
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return this.createIFrame();
  }

  @Watch('src')
  async onSrcUpdate() {
    await this.createIFrame();
  }

  @Watch('theme')
  async onThemeUpdate() {
    await this.createIFrame();
  }

  private async createIFrame() {
    if (!this.src) {
      return;
    }

    if (this.loading) {
      return;
    }

    this.loading = true;
    this.loaded = false;

    this.formattedSrc = await formatPlaygroundSrc(this.src, this.theme);

    if (this.iframe) {
      // Refresh iFrame
      this.iframe.src = this.iframe.src;
    }
  }

  private onFrameLoaded() {
    this.loading = false;
    this.loaded = true;
  }

  render() {
    const hostClass: string = this.loaded ? 'loaded' : '';
    return (
      <Host class={hostClass}>
        <div class="playground-container">{this.renderFrame()}</div>
      </Host>
    );
  }

  private renderFrame() {
    if (!this.formattedSrc) {
      return undefined;
    }

    // prettier-ignore
    // @ts-ignore
    return <iframe src={this.formattedSrc} allowfullscreen={this.allowFullscreen}
                   width={this.width} height={this.height}
                   frameborder={0} title={this.frameTitle}
                   onload={() => this.onFrameLoaded()}
                   ref={(el) => this.iframe = el as HTMLIFrameElement}>
    </iframe>
  }
}
