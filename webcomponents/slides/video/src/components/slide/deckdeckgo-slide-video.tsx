import { Component, Element, Event, EventEmitter, Method, Prop, h, Host } from '@stencil/core';

import {
  DeckdeckgoSlide,
  hideLazyLoadImages,
  afterSwipe,
  lazyLoadContent,
  hideAllRevealElements,
  showAllRevealElements
} from '@deckdeckgo/slide-utils';

const isVideoPlaying = (video: HTMLVideoElement): boolean =>
  !!(video.currentTime > 0 && !video.paused && !video.ended && video.readyState > 2);

@Component({
  tag: 'deckgo-slide-video',
  styleUrl: 'deckdeckgo-slide-video.scss',
  shadow: true
})
export class DeckdeckgoSlideVideo implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({ reflectToAttr: true }) customActions: boolean = false;
  @Prop({ reflectToAttr: true }) customBackground: boolean = false;

  private video: HTMLVideoElement;
  private videoPlayed = false;

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.video = this.el.querySelector('video');

    this.slideDidLoad.emit();
  }

  /**
   * play when swipping forward
   * always show previous slide when swipping backward
   * reset when leaving the slide
   * only show next slide if video was played and then paused or ended
   */
  @Method()
  async beforeSwipe(enter: boolean): Promise<boolean> {
    const videoPlaying = isVideoPlaying(this.video);

    const couldSwipe: boolean = !enter || (this.videoPlayed && !videoPlaying);

    if (couldSwipe) {
      this.video.pause();
      this.video.currentTime = 0;
      this.videoPlayed = false;
      // FIXME: can't exit slide sometimes ¯\_(ツ)_/¯
      // this may be caused by how this promise is prioritized by the deck given the promised stay resolved with true in this case
      return true;
    }

    if (!videoPlaying) {
      this.video.play();
      this.videoPlayed = true;
    } else {
      this.video.pause();
    }

    return false;
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
    return showAllRevealElements(this.el);
  }

  @Method()
  hideContent(): Promise<void> {
    return hideAllRevealElements(this.el);
  }

  render() {
    return (
      <Host class={{ 'deckgo-slide-container': true }}>
        <div class="deckgo-slide">
          <slot></slot>
        </div>
      </Host>
    );
  }
}
