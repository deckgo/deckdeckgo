import {Component, Element, Method, Prop, h, Watch, State} from '@stencil/core';

import {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-youtube',
  styleUrl: 'deckdeckgo-youtube.scss',
  shadow: true
})
export class DeckdeckgoYoutube implements DeckdeckgoComponent {

  @Element() el: HTMLElement;

  @Prop() src: string;

  @Prop() width: number;
  @Prop() height: number;

  @Prop() frameTitle: string;

  @State()
  private loading: boolean = false;

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

  @Watch('src')
  async onSrcUpdate() {
    await this.createIFrame();
  }

  private createIFrame(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.src) {
        resolve();
        return;
      }

      if (this.loading) {
        resolve();
        return;
      }

      const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');

      if (iframe && !iframe.parentElement) {
        resolve();
        return;
      }

      this.loading = true;

      if (iframe) {
        iframe.parentElement.removeChild(iframe);
      }

      const element: HTMLIFrameElement = document.createElement('iframe');

      const allow: Attr = document.createAttribute('allow');
      allow.value = 'accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture';

      const allowFullScreen: Attr = document.createAttribute('allowfullscreen');
      allowFullScreen.value = '';

      element.setAttributeNode(allow);
      element.setAttributeNode(allowFullScreen);

      element.src = await this.formatSrc();
      element.width = '' + this.width;
      element.height = '' + this.height;
      element.frameBorder = '0';
      element.title = this.frameTitle;

      const div: HTMLElement = this.el.shadowRoot.querySelector('div');

      if (!div) {
        resolve();
        return;
      }

      div.appendChild(element);

      this.loading = false;

      resolve();
    });
  }

  private formatSrc(): Promise<string> {
    return new Promise<string>(async (resolve) => {
      // Direct URL can't be embedded, like https://www.youtube.com/watch?v=oUOjJIfPIjw or https://youtu.be/e63Cgln6Yag
      const videoId: string = await this.findVideoId();

      if (videoId) {
        // In such a case return a link which could be embedded
        resolve('https://www.youtube.com/embed/' + videoId + '?enablejsapi=1');
      } else {
        // Otherwise we try the provided url
        resolve(this.src);
      }
    });
  }

  private findVideoId(): Promise<string> {
    return new Promise<string>((resolve) => {
      const url: URL = new URL(this.src);
      let videoId: string = url.searchParams.get('v');

      if (!videoId) {
        const host: string = url.host;
        if (host === 'youtu.be') {
          // For shortened url
          const pathname: string = url.pathname;

          if (pathname) {
            const split: string[] = url.pathname.split('/');

            if (split && split.length >= 2) {
              videoId = url.pathname.split('/')[1];
            }
          }
        }
      }

      resolve(videoId);
    });
  }

  @Method()
  play(): Promise<void> {
    return this.playPauseVideo(true);
  }

  @Method()
  pause(): Promise<void> {
    return this.playPauseVideo(false);
  }

  private playPauseVideo(play: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');

      if (!iframe) {
        resolve();
        return;
      }

      iframe.contentWindow.postMessage(JSON.stringify({
        event: 'command',
        func: play ? 'playVideo' : 'pauseVideo',
        args: ''
      }), '*');

      resolve();
    })
  }

  render() {
    return <div></div>
  }

}
