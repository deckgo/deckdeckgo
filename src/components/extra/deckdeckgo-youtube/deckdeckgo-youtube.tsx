import {Component, Element, Method, Prop} from '@stencil/core';

import {DeckdeckgoExtra} from '../deckdeckgo-extra';

@Component({
  tag: 'deckgo-youtube',
  styleUrl: 'deckdeckgo-youtube.scss',
  shadow: true
})
export class DeckdeckgoYoutube implements DeckdeckgoExtra {

  @Element() el: HTMLElement;

  @Prop() src: string;
  @Prop() width: number;
  @Prop() height: number;

  async componentDidLoad() {
    await this.addPreconnectLink();
  }

  private addPreconnectLink(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.src) {
        resolve();
        return;
      }

      const links: NodeListOf<HTMLElement> = document.head.querySelectorAll('link[rel=\'preconnect\']');

      if (links && links.length > 0) {
        resolve();
        return;
      }

      const link: HTMLLinkElement = document.createElement('link');
      link.rel = 'preconnect';
      link.href = 'https://www.youtube.com';

      document.head.appendChild(link);

      resolve();
    });
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

  private createIFrame(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.src) {
        resolve();
        return;
      }

      const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');
      if (iframe) {
        resolve();
        return;
      }

      const element: HTMLIFrameElement = document.createElement('iframe');

      const allow: Attr = document.createAttribute('allow');
      allow.value = 'accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture';

      const allowFullScreen: Attr = document.createAttribute('allowfullscreen');
      allowFullScreen.value = '';

      element.setAttributeNode(allow);
      element.setAttributeNode(allowFullScreen);

      element.src = this.formatSrc();
      element.width = '' + this.width;
      element.height = '' + this.height;
      element.frameBorder = '0';

      const div: HTMLElement = this.el.shadowRoot.querySelector('div');

      if (!div) {
        resolve();
        return;
      }

      div.appendChild(element);

      resolve();
    });
  }

  private formatSrc(): string {
    // Direct URL can't be embedded, like https://www.youtube.com/watch?v=oUOjJIfPIjw
    const url: URL = new URL(this.src);
    const videoId: string = url.searchParams.get('v');
    if (videoId) {
      // In such a case return a link which could be embedded
      return 'https://www.youtube.com/embed/' + videoId;
    } else {
      // Otherwise we try the provided url
      return this.src;
    }
  }

  render() {
    return <div></div>
  }

}
