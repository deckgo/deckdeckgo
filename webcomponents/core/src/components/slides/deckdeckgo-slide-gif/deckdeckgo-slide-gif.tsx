import {Component, Element, Event, EventEmitter, Method, Prop, Listen, State} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';

@Component({
  tag: 'deckgo-slide-gif',
  styleUrl: 'deckdeckgo-slide-gif.scss',
  shadow: true
})
export class DeckdeckgoSlideGif implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() src: string;
  @Prop() alt: string;

  @Prop() fullscreen: boolean = true;

  @State() loaded: boolean = false;

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  async componentDidLoad() {
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(_enter: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true)
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return DeckdeckgoSlideUtils.afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return DeckdeckgoSlideUtils.lazyLoadContent(this.el);
  }

  @Listen('gifLoaded')
  onGifLoaded($event: CustomEvent) {
    this.loaded = $event && $event.detail;
  }

  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <div class="deckgo-gif-container">
        <slot name="header"></slot>
        <deckgo-gif src={this.src} alt={this.alt} fullscreen={this.fullscreen}></deckgo-gif>
        <slot name="footer"></slot>
        <slot name="notes"></slot>
        <slot name="actions"></slot>
        <slot name="background"></slot>
      </div>
    </div>
  }

  hostData() {
    return {
      class: {
        'deckgo-slide-container': true,
        'deckgo-slide-container-fullscreen': this.fullscreen,
        'deckgo-slide-gif-hidden': !this.loaded
      }
    }
  }

}
