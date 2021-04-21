import { Component, h, Host, Listen, State } from "@stencil/core";

import { unifyEvent } from "@deckdeckgo/utils";

@Component({
  tag: 'deckgo-flashlight',
  styleUrl: 'flashlight.scss',
  shadow: true,
})
export class DeckgoMdParser {
  @State()
  private mouse: { x:number, y: number } | null = null;

  private idleMouseTimer: number;
  private readonly idleMouseTimeout: number = 2000;

  disconnectedCallback() {
    if (this.idleMouseTimer > 0) {
      clearTimeout(this.idleMouseTimer);
    }
  }

  @Listen('mousemove', {passive: true, target: 'document'})
  mousemove($event: MouseEvent) {
    this.move($event);
  }

  @Listen('touchmove', {passive: true, target: 'document'})
  touchmove($event: TouchEvent) {
    this.move($event);
  }

  private move($event: Event) {
    this.clearMouseTimeout();

    this.mouse = {
      x: unifyEvent($event).clientX,
      y: unifyEvent($event).clientY
    };

    this.idleMouseTimer = setTimeout(() => {
      this.mouse = null;
    }, this.idleMouseTimeout);
  }

  private clearMouseTimeout() {
    if (this.idleMouseTimer > 0) {
      clearTimeout(this.idleMouseTimer);
    }
  }

  render() {
    return (
      <Host style={{'--x': `${this.mouse?.x}px`, '--y': `${this.mouse?.y}px`}} class={this.mouse !== null ? 'show' : 'hidden'}>
      </Host>
    );
  }
}
