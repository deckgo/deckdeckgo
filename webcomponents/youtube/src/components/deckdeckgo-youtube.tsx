import {Component, Element, Method, Host, Prop, h, Watch, State} from '@stencil/core';

import type {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-youtube',
  styleUrl: 'deckdeckgo-youtube.scss',
  shadow: true
})
export class DeckdeckgoYoutube implements DeckdeckgoComponent {
  @Element() el: HTMLElement;

  /**
   * The source url, the YouTube url, of the video. Not embeddable url will be automatically converted to embeddable url supported by YouTube
   */
  @Prop()
  src: string;

  /**
   * The width of the video player
   */
  @Prop()
  width: number;
  /**
   * The height of the video player
   */
  @Prop()
  height: number;

  /**
   * A title for the frame, could be use for accessibility reason
   */
  @Prop()
  frameTitle: string;

  /**
   * Allow option to toggle video in full screen
   */
  @Prop()
  allowFullscreen: boolean = true;

  /**
   * In case you would like to load the video as soon as the component is loaded
   */
  @Prop()
  instant: boolean = false;

  @State()
  private loading: boolean = false;

  @State()
  private loaded: boolean = false;

  async componentWillLoad() {
    await this.addPreconnectLink();
  }

  async componentDidLoad() {
    if (this.instant) {
      await this.lazyLoadContent();
    }
  }

  /**
   * Update the iFrame, the video, size
   * @param width
   * @param height
   */
  @Method()
  async updateIFrame(width: number, height: number) {
    const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');

    if (iframe) {
      iframe.width = '' + width;
      iframe.height = '' + height;
    }
  }

  /**
   * Lazy load the video
   */
  @Method()
  lazyLoadContent(): Promise<void> {
    return this.createIFrame();
  }

  @Watch('src')
  async onSrcUpdate() {
    await this.createIFrame();
  }

  /**
   * Play the video
   */
  @Method()
  play(): Promise<void> {
    return this.playPauseVideo(true);
  }

  /**
   * Pause the video
   */
  @Method()
  pause(): Promise<void> {
    return this.playPauseVideo(false);
  }

  private addPreconnectLink(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.src || !document) {
        resolve();
        return;
      }

      const links: NodeListOf<HTMLElement> = document.head.querySelectorAll("link[rel='preconnect'][youtube]");

      if (links && links.length > 0) {
        resolve();
        return;
      }

      const link: HTMLLinkElement = document.createElement('link');
      link.rel = 'preconnect';
      link.href = 'https://www.youtube.com';
      link.setAttribute('youtube', '');

      document.head.appendChild(link);

      resolve();
    });
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
      this.loaded = false;

      if (iframe) {
        iframe.parentElement.removeChild(iframe);
      }

      const element: HTMLIFrameElement = document.createElement('iframe');

      const allow: Attr = document.createAttribute('allow');
      allow.value = 'accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture';
      this.setAttributeNode(element, allow);

      if (this.allowFullscreen) {
        const allowFullScreen: Attr = document.createAttribute('allowfullscreen');
        allowFullScreen.value = '';
        this.setAttributeNode(element, allowFullScreen);
      }

      let src: string = await this.formatSrc();

      if (!this.allowFullscreen) {
        // Not auto fullscreen on iOS: https://developers.google.com/youtube/player_parameters
        const playsinline: Attr = document.createAttribute('playsinline');
        playsinline.value = '1';
        this.setAttributeNode(element, playsinline);

        const split: string[] = src.split('?');

        src += `${split && split.length > 0 ? '&' : '?'}playsinline=1`;
      }

      element.src = src;
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
      this.loaded = true;
      resolve();
    });
  }

  private setAttributeNode(element: HTMLIFrameElement, attr: Attr) {
    // Stencil prerendering
    if ((element as any).setAttributeNode === 'function') {
      element.setAttributeNode(attr);
    }
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

        if (!this.src) {
          resolve(this.src);
          return;
        }

        // But first, if an embed link, like https://www.youtube.com/embed/PnSNT5WpauE, would be passed, a parameter to enable js should be added otherwise play and pause could not be triggered
        // Therefore we add enablejsapi if needed in any case

        const url: URL = new URL(this.src);

        if (!url.searchParams.get('enablejsapi')) {
          url.searchParams.append('enablejsapi', '1');
        }

        resolve(url.href);
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

  private playPauseVideo(play: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');

      if (!iframe) {
        resolve();
        return;
      }

      iframe.contentWindow.postMessage(
        JSON.stringify({
          event: 'command',
          func: play ? 'playVideo' : 'pauseVideo',
          args: ''
        }),
        '*'
      );

      resolve();
    });
  }

  render() {
    const hostClass: string = this.loaded ? 'loaded' : '';
    return (
      <Host class={hostClass}>
        <div class="youtube-container"></div>
      </Host>
    );
  }
}
