import {Component, h, Host, Listen, Prop, State, Element, Method} from '@stencil/core';

import {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';

import {unifyEvent} from '@deckdeckgo/utils';

@Component({
  tag: 'deckgo-dnr',
  styleUrl: 'deckdeckgo-dnr.scss',
  shadow: true
})
export class DeckdeckgoDnr implements DeckdeckgoComponent {
  @Element() el: HTMLElement;

  // Size

  @Prop({reflect: true})
  width: number;

  @Prop({reflect: true})
  height: number;

  // Position

  @Prop({reflect: true})
  top: number;

  @Prop({reflect: true})
  left: number;

  @State()
  private selected: boolean = false;

  private startX: number = null;
  private startY: number = null;
  private startWidth: number = null;
  private startHeight: number = null;
  private startTop: number = null;
  private startLeft: number = null;

  private dragging: boolean = false;
  private moving: boolean = false;

  // TODO

  // Tous les coins
  // Border
  // Afficher la taille et option turn it down
  // Keep aspect ratio
  // Disable enable, per default disable
  // z-Index on click ?

  // Valeur

  @Method()
  lazyLoadContent(): Promise<void> {
    // TODO
    return Promise.resolve();
  }

  @Listen('mousedown', {passive: true, target: 'document'})
  onMousedown($event: MouseEvent) {
    this.start($event);
  }

  @Listen('touchstart', {passive: true, target: 'document'})
  onTouchstart($event: TouchEvent) {
    this.start($event);
  }

  @Listen('mousemove', {passive: true, target: 'document'})
  onMousemove($event: MouseEvent) {
    this.moveOrResize($event);
  }

  @Listen('touchmove', {passive: true, target: 'document'})
  onTouchmove($event: TouchEvent) {
    this.moveOrResize($event);
  }

  @Listen('mouseup', {passive: true, target: 'document'})
  onMouseup(_$event: MouseEvent) {
    this.stop();
  }

  @Listen('touchend', {passive: true, target: 'document'})
  onTouchend(_$event: TouchEvent) {
    this.stop();
  }

  private start($event: MouseEvent | TouchEvent) {
    if (!$event || !$event.target) {
      return;
    }

    const selected: HTMLElement = ($event.target as HTMLElement).closest('deckgo-dnr');

    // If we click elsewhere or select another component, then this component should loose focus and values need to be reset for next usage
    if (!selected || !selected.isEqualNode(this.el)) {
      this.stop();
      this.selected = false;
      return;
    }

    this.selected = true;

    this.startMove($event);
    this.startResize($event);
  }

  private moveOrResize($event: MouseEvent | TouchEvent) {
    this.move($event);
    this.resize($event);
  }

  private move($event: MouseEvent | TouchEvent) {
    if (!this.moving) {
      return;
    }

    const delta: {x: number; y: number} = this.getDelta($event);

    this.left = this.startLeft + delta.x > 0 ? this.startLeft + delta.x : 0;
    this.top = this.startTop + delta.y > 0 ? this.startTop + delta.y : 0;
  }

  private resize($event: MouseEvent | TouchEvent) {
    if (!this.selected || !this.startX || !this.startY || !this.startWidth || !this.startHeight || !this.dragging) {
      return;
    }

    const delta: {x: number; y: number} = this.getDelta($event);

    this.width = this.startWidth + delta.x > 0 ? this.startWidth + delta.x : 0;
    this.height = this.startHeight + delta.y > 0 ? this.startHeight + delta.y : 0;
  }

  private getDelta($event: MouseEvent | TouchEvent): {x: number; y: number} {
    const currentX: number = unifyEvent($event).clientX;
    const currentY: number = unifyEvent($event).clientY;

    const deltaX: number = currentX - this.startX;
    const deltaY: number = currentY - this.startY;

    return {
      x: deltaX,
      y: deltaY
    };
  }

  private startResize($event: MouseEvent | TouchEvent) {
    this.startX = unifyEvent($event).clientX;
    this.startY = unifyEvent($event).clientY;
    this.startWidth = this.width;
    this.startHeight = this.height;
  }

  private startMove($event: MouseEvent | TouchEvent) {
    if (this.dragging) {
      return;
    }

    this.moving = true;

    this.startX = unifyEvent($event).clientX;
    this.startY = unifyEvent($event).clientY;
    this.startTop = this.top;
    this.startLeft = this.left;
  }

  private stop() {
    this.startX = null;
    this.startY = null;

    this.stopMove();
    this.stopResize();
  }

  private stopMove() {
    this.moving = false;

    this.startTop = null;
    this.startLeft = null;
  }

  private stopResize() {
    this.dragging = false;

    this.startWidth = null;
    this.startHeight = null;
  }

  render() {
    return (
      <Host
        style={{'--width': `${this.width}px`, '--height': `${this.height}px`, '--top': `${this.top}px`, '--left': `${this.left}px`}}
        class={this.selected ? 'selected' : undefined}>
        {this.renderAnchors()}
        <slot />
      </Host>
    );
  }

  private renderAnchors() {
    if (!this.selected) {
      return undefined;
    }

    return [<div class="anchor" onClick={($event) => $event.stopPropagation()} onMouseDown={() => (this.dragging = true)}></div>];
  }
}
