import {Component, h, Prop, State, Element, Method, Watch, ComponentInterface} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';

@Component({
  tag: 'deckgo-demo',
  styleUrl: 'demo.scss',
  shadow: true
})
export class Demo implements ComponentInterface {
  @Element() el: HTMLElement;

  /**
   * The source Url of your application or website. This will be used as src attribute of the encapsulated iframe
   */
  @Prop({reflect: true}) src: string;

  /**
   * A title for the frame, could be use for accessibility reason
   */
  @Prop({reflect: true}) frameTitle: string;

  /**
   * The type of device frame. md for Android, ios for iPhone
   */
  @Prop({reflect: true}) mode = 'md';

  /**
   * In case you would like to load the frame as soon as the component is loaded
   */
  @Prop() instant: boolean = false;

  @State()
  private width: number;

  private height: number;

  @State()
  private loading: boolean = false;

  @State()
  private loaded: boolean = false;

  container!: HTMLElement;

  async componentDidLoad() {
    this.initWindowResize();

    await this.initSize();

    if (this.instant) {
      await this.lazyLoadContent();
    }
  }

  disconnectedCallback() {
    if (window) {
      window.removeEventListener('resize', debounce(this.onResizeContent, 500));
    }
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onResizeContent, 500));
    }
  }

  private onResizeContent = async () => {
    await this.resizeReload();
  };

  /**
   * Refresh iframe size and reload content
   */
  @Method()
  async updateIFrame() {
    await this.resizeReload();
  }

  private async resizeReload() {
    await this.initSize();
    await this.updateIFrameSizeReload();
  }

  private async initSize() {
    const style: CSSStyleDeclaration | undefined = window ? window.getComputedStyle(this.el) : undefined;

    const width: number = style && parseInt(style.width) > 0 ? parseInt(style.width) : this.el.offsetWidth;
    const height: number = style && parseInt(style.height) > 0 ? parseInt(style.height) : this.el.offsetHeight;

    const deviceHeight: number = (width * 704) / 304;

    this.width = deviceHeight > height ? (height * 304) / 704 : width;
    this.height = deviceHeight > height ? height : deviceHeight;
  }

  /**
   * Lazy load the iframe
   */
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
      if (!this.src || !this.container) {
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

      // Stencil prerendering
      if ((element as any).setAttributeNode === 'function') {
        const allow: Attr = document.createAttribute('allow');
        allow.value = 'accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture';
        element.setAttributeNode(allow);
      }

      element.src = this.src;
      element.width = '' + this.width;
      element.height = '' + this.height;
      element.title = this.frameTitle;
      element.frameBorder = '0';

      this.container.appendChild(element);

      this.loaded = true;
      this.loading = false;

      resolve();
    });
  }

  private async updateIFrameSizeReload() {
    const iframe: HTMLIFrameElement = this.el.shadowRoot.querySelector('iframe');

    if (iframe) {
      iframe.width = '' + this.width;
      iframe.height = '' + this.height;

      // Reload a cross-domain frame. Accessing the content is not allowed because of the CSP cross-origin restriction.
      iframe.src = iframe.src;
    }
  }

  render() {
    return (
      <div class={`demo-device ${this.mode}`} style={{'--auto-size': `${this.width}px`}}>
        <figure ref={(el) => (this.container = el as HTMLElement)}>
          <svg class="demo-device__md-bar" viewBox="0 0 1384.3 40.3">
            <path class="st0" d="M1343 5l18.8 32.3c.8 1.3 2.7 1.3 3.5 0L1384 5c.8-1.3-.2-3-1.7-3h-37.6c-1.5 0-2.5 1.7-1.7 3z" />
            <circle class="st0" cx="1299" cy="20.2" r="20" />
            <path
              class="st0"
              d="M1213 1.2h30c2.2 0 4 1.8 4 4v30c0 2.2-1.8 4-4 4h-30c-2.2 0-4-1.8-4-4v-30c0-2.3 1.8-4 4-4zM16 4.2h64c8.8 0 16 7.2 16 16s-7.2 16-16 16H16c-8.8 0-16-7.2-16-16s7.2-16 16-16z"
            />
          </svg>
          <svg class="demo-device__ios-notch" viewBox="0 0 219 31">
            <path d="M0 1V0h219v1a5 5 0 0 0-5 5v3c0 12.15-9.85 22-22 22H27C14.85 31 5 21.15 5 9V6a5 5 0 0 0-5-5z" fill-rule="evenodd" />
          </svg>
          {!this.loaded ? <div class="placeholder"></div> : undefined}
        </figure>
      </div>
    );
  }
}
