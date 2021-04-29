import {Component, Element, Event, EventEmitter, Method, Prop, h, Host, State, Watch} from '@stencil/core';

import {isMobile} from '@deckdeckgo/utils';

import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

/**
 * @slot title - A title
 * @slot author - Author of the slides
 * @slot social-link - Several optional social links
 * @slot notes - Some notes related to this slide
 * @slot actions - Custom actions for this slide
 * @slot background - A custom background for this slide
 * @slot header - A custom header for this slide
 * @slot footer - A custom footer for this slide
 */
@Component({
  tag: 'deckgo-slide-author',
  styleUrl: 'deckdeckgo-slide-author.scss',
  shadow: true,
})
export class DeckdeckgoSlideAuthor implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  /**
   * Triggered when the slide is loaded
   */
  @Event() slideDidLoad: EventEmitter<void>;

  /**
   * An image URI, for example a picture of the author. Note: this image will be displayed as a circle.
   */
  @Prop() imgSrc: string;
  /**
   * An optional accessibility alt for the image.
   */
  @Prop() imgAlt: string;

  /**
   * The design to be applied to the image. cover fits the image to the start pane, circle displays it in a circle and none in case you would not like to display an image
   */
  @Prop({reflect: true}) imgMode: 'cover' | 'circle' | 'none' = 'cover';

  @State()
  private mobile: boolean = false;

  @State()
  private isLazyLoaded: boolean = false;

  private lazyLoadAfterUpdate: boolean = false;

  /**
   * If you define a background for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customBackground: boolean = false;

  /**
   * If you provide actions for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customActions: boolean = false;

  componentWillLoad() {
    this.mobile = isMobile();
  }

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();
  }

  @Watch('imgMode')
  async onPropChanges() {
    this.lazyLoadAfterUpdate = true;
  }

  async componentDidUpdate() {
    if (this.lazyLoadAfterUpdate) {
      await this.lazyLoadContent();
      this.lazyLoadAfterUpdate = false;
    }
  }

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true);
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await lazyLoadContent(this.el);

      this.isLazyLoaded = true;

      resolve();
    });
  }

  @Method()
  revealContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  hideContent(): Promise<void> {
    return Promise.resolve();
  }

  render() {
    const classAuthorStart: string = `deckgo-slide-author deckgo-slide-author-start deckgo-slide-author-${this.imgMode} ${
      this.mobile ? 'deckgo-slide-author-mobile' : ''
    }`;
    const classAuthorEnd: string = `deckgo-slide-author deckgo-slide-author-end deckgo-slide-author-${this.imgMode} ${
      this.mobile ? 'deckgo-slide-author-mobile' : ''
    }`;

    return (
      <Host class={{'deckgo-slide-container': true}}>
        <div class="deckgo-slide">
          <slot name="title"></slot>
          <div class={classAuthorStart} style={{'--slide-author-color-start-img-url': this.isLazyLoaded ? `url(${this.imgSrc})` : undefined}}>
            {this.renderImage()}
          </div>
          <div class={classAuthorEnd}>
            <slot name="author"></slot>
            <div class="deckgo-slide-author-social">
              <slot name="social-link"></slot>
              <slot name="social-link"></slot>
              <slot name="social-link"></slot>
              <slot name="social-link"></slot>
              <slot name="social-link"></slot>
              <slot name="social-link"></slot>
            </div>
          </div>
          <slot name="notes"></slot>
          <slot name="actions"></slot>
          <slot name="background"></slot>
          <slot name="header"></slot>
          <slot name="footer"></slot>
        </div>
      </Host>
    );
  }

  private renderImage() {
    if (this.imgMode === 'cover' || this.imgMode === 'none') {
      return undefined;
    }

    return <img data-src={this.imgSrc} alt={this.imgAlt} />;
  }
}
