import {Component, Element, Event, EventEmitter, Method, Prop, h, Host, State} from '@stencil/core';

import {isMobile} from '@deckdeckgo/utils';

import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-slide-author',
  styleUrl: 'deckdeckgo-slide-author.scss',
  shadow: true
})
export class DeckdeckgoSlideAuthor implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() imgSrc: string;
  @Prop() imgAlt: string;

  @Prop({reflectToAttr: true}) imgMode: 'cover' | 'circle' | 'none' = 'cover';

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  @State()
  private mobile: boolean = false;

  componentWillLoad() {
    this.mobile = isMobile();
  }

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true)
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return lazyLoadContent(this.el);
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
    const classAuthorStart: string = `deckgo-slide-author deckgo-slide-author-start deckgo-slide-author-${this.imgMode} ${this.mobile ? 'deckgo-slide-author-mobile' : ''}`;
    const classAuthorEnd: string = `deckgo-slide-author deckgo-slide-author-end deckgo-slide-author-${this.imgMode} ${this.mobile ? 'deckgo-slide-author-mobile' : ''}`;

    return <Host class={{'deckgo-slide-container': true}}>
      <div class="deckgo-slide">
        <slot name="title"></slot>
        <div class={classAuthorStart} style={{'--slide-author-color-start-img-url': `url(${this.imgSrc})`}}>
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
      </div>
    </Host>;
  }

  private renderImage() {
    if (this.imgMode === 'cover' || this.imgMode === 'none') {
      return undefined;
    }

    return <img data-src={this.imgSrc} alt={this.imgAlt}/>;
  }

}
