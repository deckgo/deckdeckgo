import {Component, h, Host, Listen, Prop, State, Element, Method} from '@stencil/core';

import {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';

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

  @State()
  private doTheJob: boolean = false;

  private startX: number = null;
  private startY: number = null;
  private startWidth: number = null;
  private startHeight: number = null;
  private startTop: number = null;
  private startLeft: number = null;

  private dragging: boolean = false;

  // Position

  @Prop({reflect: true})
  top: number;

  @Prop({reflect: true})
  left: number;

  private moving: boolean = false;

  // TODO

  // Tous les coins
  // Border
  // Afficher la taille et option turn it down
  // Keep aspect ratio
  // Disable enable
  // z-Index on click ?

  // Valeur

  @Method()
  lazyLoadContent(): Promise<void> {
    // TODO
    return Promise.resolve();
  }

  @Listen('mousedown', {passive: true, target: 'document'})
  onMousedown($event: MouseEvent) {
    const selected = ($event.target as HTMLElement).closest('deckgo-dnr');

    if (!selected || !(selected as any).isEqualNode(this.el)) {
      this.stopResize();
      this.stopMove();
      this.doTheJob = false;
      return;
    }

    this.doTheJob = true;

    this.startMove($event);
    this.startResize($event);
  }

  @Listen('mouseup', {passive: true, target: 'document'})
  onMouseup(_$event: MouseEvent) {
    this.stopMove();

    this.stopResize();
  }

  @Listen('mousemove', {passive: true, target: 'document'})
  onMousemove($event: MouseEvent) {
    this.move($event);

    this.resize($event);
  }

  private move($event: MouseEvent) {
    if (!this.moving) {
      return;
    }

    const currentX: number = $event.clientX;
    const currentY: number = $event.clientY;

    const deltaX: number = currentX - this.startX;
    const deltaY: number = currentY - this.startY;

    this.left = this.startLeft + deltaX > 0 ? this.startLeft + deltaX : 0;
    this.top = this.startTop + deltaY > 0 ? this.startTop + deltaY : 0;
  }

  private resize($event: MouseEvent) {
    if (!this.doTheJob || !this.startX || !this.startY || !this.startWidth || !this.startHeight || !this.dragging) {
      return;
    }

    const currentX: number = $event.clientX;
    const currentY: number = $event.clientY;

    const deltaX: number = currentX - this.startX;
    const deltaY: number = currentY - this.startY;

    this.width = this.startWidth + deltaX > 0 ? this.startWidth + deltaX : 0;
    this.height = this.startHeight + deltaY > 0 ? this.startHeight + deltaY : 0;
  }

  private stopResize() {
    this.dragging = false;

    this.startX = null;
    this.startY = null;
    this.startWidth = null;
    this.startHeight = null;
  }

  private stopMove() {
    this.moving = false;

    this.startX = null;
    this.startY = null;
    this.startTop = null;
    this.startLeft = null;
  }

  private startResize(e: MouseEvent) {
    this.startX = e.clientX;
    this.startY = e.clientY;
    this.startWidth = this.width;
    this.startHeight = this.height;
  }

  private startMove(e: MouseEvent) {
    if (this.dragging) {
      return;
    }

    this.moving = true;

    this.startX = e.clientX;
    this.startY = e.clientY;
    this.startTop = this.top;
    this.startLeft = this.left;
  }

  render() {
    return (
      <Host
        style={{'--width': `${this.width}px`, '--height': `${this.height}px`, '--top': `${this.top}px`, '--left': `${this.left}px`}}
        class={this.doTheJob ? 'selected' : undefined}>
        {this.renderAnchors()}
        <slot />
      </Host>
    );
  }

  private renderAnchors() {
    if (!this.doTheJob) {
      return undefined;
    }

    return [<div class="anchor" onClick={($event) => $event.stopPropagation()} onMouseDown={() => (this.dragging = true)}></div>];
  }
}
