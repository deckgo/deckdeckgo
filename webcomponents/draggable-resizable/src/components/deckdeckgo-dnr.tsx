import {Component, h, Host, Prop, State, Element, Method} from '@stencil/core';

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

  @Prop()
  resize: boolean = true;

  @Prop({reflect: true})
  width: number;

  @Prop({reflect: true})
  height: number;

  @Prop()
  minWidth: number = 32;

  @Prop()
  minHeight: number = 32;

  // Position

  @Prop()
  drag: 'x-axis' | 'y-axis' | 'all' | 'none' = 'all';

  @Prop({reflect: true})
  top: number;

  @Prop({reflect: true})
  left: number;

  @State()
  private selected: boolean = false;

  @State()
  private moving: boolean = false;

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

  // TODO

  // Tous les coins
  // En pixel ou pourcentage
  // Afficher la taille et option turn it down
  // Keep aspect ratio
  // Disable enable, per default disable
  // z-Index on click ?

  // Valeur

  componentWillLoad() {
    if (document && (this.drag || this.resize)) {
      document.addEventListener('mousedown', this.start.bind(this), {passive: true});
      document.addEventListener('touchstart', this.start.bind(this), {passive: true});
      document.addEventListener('mousemove', this.moveOrResize.bind(this), {passive: true});
      document.addEventListener('touchmove', this.moveOrResize.bind(this), {passive: true});
      document.addEventListener('mouseup', this.stop.bind(this), {passive: true});
      document.addEventListener('touchend', this.stop.bind(this), {passive: true});
    }
  }

  async componentDidLoad() {
    await this.displaySlot();
  }

  componentWillUnload() {
    if (document && (this.drag || this.resize)) {
      document.removeEventListener('mousedown', this.start, true);
      document.removeEventListener('touchstart', this.start, true);
      document.removeEventListener('mousemove', this.moveOrResize, true);
      document.removeEventListener('touchmove', this.moveOrResize, true);
      document.removeEventListener('mouseup', this.stop, true);
      document.removeEventListener('touchend', this.stop, true);
    }
  }

  private async displaySlot() {
    const element: HTMLElement = this.el.querySelector(`:scope > [slot]`);

    if (element && element.style.display === 'none') {
      element.style.display = '';
    }
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    // TODO
    return Promise.resolve();
  }

  private start = ($event: MouseEvent | TouchEvent) => {
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
  };

  private moveOrResize($event: MouseEvent | TouchEvent) {
    this.move($event);
    this.size($event);
  }

  private move = ($event: MouseEvent | TouchEvent) => {
    if (!this.moving || this.drag === 'none') {
      return;
    }

    if (this.drag === 'x-axis') {
      this.delta($event, {left: ApplyOperation.ADD});
    } else if (this.drag === 'y-axis') {
      this.delta($event, {top: ApplyOperation.ADD});
    } else {
      this.delta($event, {top: ApplyOperation.ADD, left: ApplyOperation.ADD});
    }
  };

  private size($event: MouseEvent | TouchEvent) {
    if (!this.resize) {
      return;
    }

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

  private stop = () => {
    this.startX = null;
    this.startY = null;

    this.stopMove();
    this.stopResize();
  };

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
        class={`${this.selected ? 'selected' : ''} ${this.drag !== 'none' ? 'draggable' : ''} ${this.drag !== 'none' && this.moving ? 'drag' : ''}`}>
        {this.renderAnchors()}
        <slot />
      </Host>
    );
  }

  private renderAnchors() {
    if (!this.selected || !this.resize) {
      return undefined;
    }

    return [
      <div
        class="anchor top end"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragTopEnd = true)}
        onTouchStart={() => (this.dragTopEnd = true)}>
        <div></div>
      </div>,
      <div
        class="anchor bottom end"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragBottomEnd = true)}
        onTouchStart={() => (this.dragBottomEnd = true)}>
        <div></div>
      </div>,
      <div
        class="anchor bottom start"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragBottomStart = true)}
        onTouchStart={() => (this.dragBottomStart = true)}>
        <div></div>
      </div>,
      <div
        class="anchor top start"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragTopStart = true)}
        onTouchStart={() => (this.dragTopStart = true)}>
        <div></div>
      </div>
    ];
  }
}
