import {Component, Element, Prop, Method, State, Event, EventEmitter, h, Host} from '@stencil/core';

import {DeckdeckgoComponent, hideLazyLoadImages, lazyLoadImages} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-gif',
  styleUrl: 'deckdeckgo-gif.scss',
  shadow: true,
})
export class DeckdeckgoGif implements DeckdeckgoComponent {
  @Element() el: HTMLElement;

  @Prop() src: string;
  @Prop() alt: string;

  @Prop() fullscreen: boolean = false;

  @State() loaded: boolean = false;

  @Event() gifLoaded: EventEmitter<boolean>;

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');

    if (img) {
      img.addEventListener(
        'load',
        () => {
          this.loaded = true;
          this.gifLoaded.emit(true);
        },
        false
      );
    }
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return lazyLoadImages(this.el);
  }

  render() {
    return (
      <Host
        class={{
          'deckgo-gif-fullscreen': this.fullscreen,
          'deckgo-gif-hidden': !this.loaded,
        }}>
        <div class="deckgo-gif">
          <slot name="top"></slot>
          <img data-src={this.src} alt={this.alt} />
          <slot name="bottom"></slot>
        </div>
      </Host>
    );
  }
}
