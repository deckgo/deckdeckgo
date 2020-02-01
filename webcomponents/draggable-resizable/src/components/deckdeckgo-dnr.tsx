import {Component, h, Host, Listen, Prop, State, Element, Method} from '@stencil/core';

import {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';

import {unifyEvent} from '@deckdeckgo/utils';

enum ApplyOperation {
  ADD,
  SUBSTRACT
}

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

  @Prop()
  minWidth: number = 32;

  @Prop()
  minHeight: number = 32;

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

  private dragTopEnd: boolean = false;
  private dragBottomEnd: boolean = false;
  private dragBottomStart: boolean = false;
  private dragTopStart: boolean = false;

  private moving: boolean = false;

  // TODO

  // Tous les coins
  // En pixel ou pourcentage
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

    this.startMove();
    this.initStartPositions($event);
  }

  private moveOrResize($event: MouseEvent | TouchEvent) {
    this.move($event);
    this.resize($event);
  }

  private move($event: MouseEvent | TouchEvent) {
    if (!this.moving) {
      return;
    }

    this.delta($event, {top: ApplyOperation.ADD, left: ApplyOperation.ADD});
  }

  private resize($event: MouseEvent | TouchEvent) {
    if (!this.selected || !this.startX || !this.startY || !this.startWidth || !this.startHeight) {
      return;
    }

    if (!this.dragBottomEnd && !this.dragTopEnd && !this.dragBottomStart && !this.dragTopStart) {
      return;
    }

    if (this.dragBottomEnd) {
      this.delta($event, {width: ApplyOperation.ADD, height: ApplyOperation.ADD});
    } else if (this.dragTopEnd) {
      this.delta($event, {width: ApplyOperation.ADD, height: ApplyOperation.SUBSTRACT, top: ApplyOperation.ADD});
    } else if (this.dragBottomStart) {
      this.delta($event, {width: ApplyOperation.SUBSTRACT, height: ApplyOperation.ADD, left: ApplyOperation.ADD});
    } else if (this.dragTopStart) {
      this.delta($event, {width: ApplyOperation.SUBSTRACT, top: ApplyOperation.ADD, height: ApplyOperation.SUBSTRACT, left: ApplyOperation.ADD});
    }
  }

  private delta($event: MouseEvent | TouchEvent, attr: {width?: ApplyOperation; height?: ApplyOperation; top?: ApplyOperation; left?: ApplyOperation}) {
    const delta: {x: number; y: number} = this.getDelta($event);

    if (attr.width === ApplyOperation.ADD) {
      this.width = this.startWidth + delta.x > this.minWidth ? this.startWidth + delta.x : this.minWidth;
    } else if (attr.width === ApplyOperation.SUBSTRACT) {
      this.width = this.startWidth - delta.x > this.minWidth ? this.startWidth - delta.x : this.minWidth;
    }

    if (attr.height === ApplyOperation.ADD) {
      this.height = this.startHeight + delta.y > this.minHeight ? this.startHeight + delta.y : this.minHeight;
    } else if (attr.height === ApplyOperation.SUBSTRACT) {
      this.height = this.startHeight - delta.y > this.minHeight ? this.startHeight - delta.y : this.minHeight;
    }

    if (attr.top === ApplyOperation.ADD) {
      this.top = this.startTop + delta.y > 0 ? this.startTop + delta.y : 0;
    }

    if (attr.left === ApplyOperation.ADD) {
      this.left = this.startLeft + delta.x > 0 ? this.startLeft + delta.x : 0;
    }
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

  private initStartPositions($event: MouseEvent | TouchEvent) {
    this.startX = unifyEvent($event).clientX;
    this.startY = unifyEvent($event).clientY;

    this.startWidth = this.width;
    this.startHeight = this.height;

    this.startTop = this.top;
    this.startLeft = this.left;
  }

  private startMove() {
    if (this.dragBottomEnd || this.dragTopEnd || this.dragBottomStart || this.dragTopStart) {
      return;
    }

    this.moving = true;
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
    this.dragBottomEnd = false;
    this.dragTopEnd = false;
    this.dragBottomStart = false;
    this.dragTopStart = false;

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

    return [
      <div class="anchor top end" onClick={($event) => $event.stopPropagation()} onMouseDown={() => (this.dragTopEnd = true)}></div>,
      <div class="anchor bottom end" onClick={($event) => $event.stopPropagation()} onMouseDown={() => (this.dragBottomEnd = true)}></div>,
      <div class="anchor bottom start" onClick={($event) => $event.stopPropagation()} onMouseDown={() => (this.dragBottomStart = true)}></div>,
      <div class="anchor top start" onClick={($event) => $event.stopPropagation()} onMouseDown={() => (this.dragTopStart = true)}></div>
    ];
  }
}
