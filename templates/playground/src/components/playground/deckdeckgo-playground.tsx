import {Component, Element, Method, Host, Prop, h, Watch, State} from '@stencil/core';

import type {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';

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
  private loaded: boolean = false;

  private loading: boolean = false;

  async componentDidLoad() {
    if (this.instant) {
      await this.lazyLoadContent();
    }
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return this.createIFrame();
  }

  @Watch('src')
  async onSrcUpdate() {
    await this.updateIFrame();
  }

  @Watch('theme')
  async onThemeUpdate() {
    await this.updateIFrame();
  }

  private async updateIFrame() {
    if (!this.src) {
      return;
    }

    const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');

    if (iframe) {
      const formattedSrc: string = await formatPlaygroundSrc(this.src, this.theme);

      iframe.src = formattedSrc;

      // Refresh iFrame
      iframe.src = iframe.src;
    } else {
      await this.createIFrame();
    }
  }

  private async createIFrame() {
    if (!this.src) {
      return;
    }

    if (this.loading) {
      return;
    }

    const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');

    if (iframe) {
      return;
    }

    this.loaded = false;
    this.loading = true;

    if (iframe) {
      iframe.parentElement.removeChild(iframe);
    }

    const element: HTMLIFrameElement = document.createElement('iframe');

    if (this.allowFullscreen) {
      const allowFullScreen: Attr = document.createAttribute('allowfullscreen');
      allowFullScreen.value = '';
      this.setAttributeNode(element, allowFullScreen);
    }

    const src: string = await formatPlaygroundSrc(this.src, this.theme);

    if (!src) {
      return;
    }

    element.src = src;
    element.frameBorder = '0';
    element.title = this.frameTitle;

    const div: HTMLElement = this.el.shadowRoot.querySelector('div');

    if (!div) {
      return;
    }

    div.appendChild(element);

    this.loaded = true;
    this.loading = false;
  }

  private setAttributeNode(element: HTMLIFrameElement, attr: Attr) {
    // Stencil prerendering
    if ((element as any).setAttributeNode === 'function') {
      element.setAttributeNode(attr);
    }
  }

  render() {
    const hostClass: string = this.loaded ? 'loaded' : '';
    return (
      <Host class={hostClass} style={{'--playground-width': `${this.width}px`, '--playground-height': `${this.height}px`}}>
        <div class="playground-container"></div>
      </Host>
    );
  }
}
