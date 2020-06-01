import {Component, Element, Method, Host, Prop, h, Watch, State} from '@stencil/core';

import {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';
import {isMobile} from '@deckdeckgo/utils';

@Component({
  tag: 'deckgo-codepen',
  styleUrl: 'deckdeckgo-codepen.scss',
  shadow: true,
})
export class DeckdeckgoCodepen implements DeckdeckgoComponent {
  @Element() el: HTMLElement;

  @Prop() src: string;

  @Prop() width: number;
  @Prop() height: number;

  @Prop() frameTitle: string;

  @Prop() allowFullscreen: boolean = true;

  @Prop() instant: boolean = false;

  @State()
  private loading: boolean = false;

  @State()
  private loaded: boolean = false;

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

  private async createIFrame() {
    if (!this.src) {
      return;
    }

    if (this.loading) {
      return;
    }

    const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');

    if (iframe && !iframe.parentElement) {
      return;
    }

    this.loading = true;
    this.loaded = false;

    if (iframe) {
      iframe.parentElement.removeChild(iframe);
    }

    const element: HTMLIFrameElement = document.createElement('iframe');

    if (this.allowFullscreen) {
      const allowFullScreen: Attr = document.createAttribute('allowfullscreen');
      allowFullScreen.value = '';
      this.setAttributeNode(element, allowFullScreen);
    }

    const src: string = await this.formatSrc();

    if (!src) {
      return;
    }

    element.src = `${src}?default-tab=result&embed-version=2`;
    element.width = '' + this.width;
    element.height = '' + this.height;
    element.frameBorder = '0';
    element.title = this.frameTitle;

    const div: HTMLElement = this.el.shadowRoot.querySelector('div');

    if (!div) {
      return;
    }

    div.appendChild(element);

    this.loading = false;
    this.loaded = true;
  }

  private setAttributeNode(element: HTMLIFrameElement, attr: Attr) {
    // Stencil prerendering
    if ((element as any).setAttributeNode === 'function') {
      element.setAttributeNode(attr);
    }
  }

  private async formatSrc(): Promise<string | undefined> {
    if (!this.src) {
      return undefined;
    }

    // On mobile device, embed pens in a preview state where they need to be clicked to loaded
    if (isMobile()) {
      return this.src.replace('/pen/', '/embed/preview/');
    } else {
      return this.src.replace('/pen/', '/embed/');
    }
  }

  render() {
    const hostClass: string = this.loaded ? 'loaded' : '';
    return (
      <Host class={hostClass}>
        <div class="codepen-container"></div>
      </Host>
    );
  }
}
