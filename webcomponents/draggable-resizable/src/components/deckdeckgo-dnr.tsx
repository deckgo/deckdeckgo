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

  @Prop({reflect: true, mutable: true})
  width: number;

  @Prop({reflect: true, mutable: true})
  height: number;

  @Prop()
  minWidth: number = 32;

  @Prop()
  minHeight: number = 32;

  // Position

  @Prop()
  drag: 'x-axis' | 'y-axis' | 'all' | 'none' = 'all';

  @Prop({reflect: true, mutable: true})
  top: number;

  @Prop({reflect: true, mutable: true})
  left: number;

  // Rotate

  @Prop()
  rotation: boolean = true;

  @Prop({reflect: true, mutable: true})
  rotate: number;

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

  private dragTop: boolean = false;
  private dragEnd: boolean = false;
  private dragBottom: boolean = false;
  private dragStart: boolean = false;

  private parentWidth: number = null;
  private parentHeight: number = null;

  private rotating: boolean = false;

  private centerX: number = null;
  private centerY: number = null;

  // TODO

  // Afficher la taille et option turn it down
  // Disable enable, per default disable
  // z-Index on click ?
  // Debounce event emitter

  // Valeur

  componentWillLoad() {
    if (document && (this.drag || this.resize)) {
      document.addEventListener('mousedown', this.start.bind(this), {passive: true});
      document.addEventListener('touchstart', this.start.bind(this), {passive: true});
      document.addEventListener('mousemove', this.transform.bind(this), {passive: true});
      document.addEventListener('touchmove', this.transform.bind(this), {passive: true});
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
      document.removeEventListener('mousemove', this.transform, true);
      document.removeEventListener('touchmove', this.transform, true);
      document.removeEventListener('mouseup', this.stop, true);
      document.removeEventListener('touchend', this.stop, true);
    }
  }

  private async displaySlot() {
    const element: HTMLElement = this.el.querySelector(`:scope > *`);

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

  private transform = ($event: MouseEvent | TouchEvent) => {
    this.move($event);
    this.size($event);
    this.rotateForm($event);
  };

  private move($event: MouseEvent | TouchEvent) {
    if (!this.moving || this.drag === 'none') {
      return;
    }

    if (this.drag === 'x-axis') {
      this.deltaMove($event, {left: ApplyOperation.ADD});
    } else if (this.drag === 'y-axis') {
      this.deltaMove($event, {top: ApplyOperation.ADD});
    } else {
      this.deltaMove($event, {top: ApplyOperation.ADD, left: ApplyOperation.ADD});
    }
  }

  private size($event: MouseEvent | TouchEvent) {
    if (!this.resize) {
      return;
    }

    if (!this.selected || !this.startX || !this.startY || !this.startWidth || !this.startHeight) {
      return;
    }

    if (this.dragBottomEnd) {
      this.deltaResize($event, {width: ApplyOperation.ADD, height: ApplyOperation.ADD});
    } else if (this.dragTopEnd) {
      this.deltaResize($event, {width: ApplyOperation.ADD, height: ApplyOperation.SUBSTRACT, top: ApplyOperation.ADD});
    } else if (this.dragBottomStart) {
      this.deltaResize($event, {width: ApplyOperation.SUBSTRACT, height: ApplyOperation.ADD, left: ApplyOperation.ADD});
    } else if (this.dragTopStart) {
      this.deltaResize($event, {width: ApplyOperation.SUBSTRACT, top: ApplyOperation.ADD, height: ApplyOperation.SUBSTRACT, left: ApplyOperation.ADD});
    } else if (this.dragTop) {
      this.deltaResize($event, {top: ApplyOperation.ADD, height: ApplyOperation.SUBSTRACT});
    } else if (this.dragEnd) {
      this.deltaResize($event, {width: ApplyOperation.ADD});
    } else if (this.dragBottom) {
      this.deltaResize($event, {height: ApplyOperation.ADD});
    } else if (this.dragStart) {
      this.deltaResize($event, {left: ApplyOperation.ADD, width: ApplyOperation.SUBSTRACT});
    }
  }

  private deltaMove($event: MouseEvent | TouchEvent, attr: {top?: ApplyOperation; left?: ApplyOperation}) {
    const delta: {x: number; y: number} = this.getDelta($event);

    if (attr.top === ApplyOperation.ADD) {
      const maxTop: number = this.parentHeight - this.startHeight;
      this.top = this.startTop + delta.y > 0 ? (this.startTop + delta.y < maxTop ? this.startTop + delta.y : maxTop) : 0;
    }

    if (attr.left === ApplyOperation.ADD) {
      const maxLeft: number = this.parentWidth - this.startWidth;
      this.left = this.startLeft + delta.x > 0 ? (this.startLeft + delta.x < maxLeft ? this.startLeft + delta.x : maxLeft) : 0;
    }
  }

  private deltaResize($event: MouseEvent | TouchEvent, attr: {width?: ApplyOperation; height?: ApplyOperation; top?: ApplyOperation; left?: ApplyOperation}) {
    const delta: {x: number; y: number} = this.getDelta($event);

    if (attr.width === ApplyOperation.ADD) {
      const maxWidth: number = this.parentWidth - this.startLeft;
      this.width = this.startWidth + delta.x > this.minWidth ? (this.startWidth + delta.x < maxWidth ? this.startWidth + delta.x : maxWidth) : this.minWidth;
    } else if (attr.width === ApplyOperation.SUBSTRACT) {
      const maxWidth: number = this.startLeft + this.startWidth;
      this.width = this.startWidth - delta.x > this.minWidth ? (this.startWidth - delta.x < maxWidth ? this.startWidth - delta.x : maxWidth) : this.minWidth;
    }

    if (attr.height === ApplyOperation.ADD) {
      const maxHeight: number = this.parentHeight - this.startTop;
      this.height =
        this.startHeight + delta.y > this.minHeight ? (this.startHeight + delta.y < maxHeight ? this.startHeight + delta.y : maxHeight) : this.minHeight;
    } else if (attr.height === ApplyOperation.SUBSTRACT) {
      const maxHeight: number = this.startTop + this.startHeight;
      this.height =
        this.startHeight - delta.y > this.minHeight ? (this.startHeight - delta.y < maxHeight ? this.startHeight - delta.y : maxHeight) : this.minHeight;
    }

    if (attr.top === ApplyOperation.ADD) {
      const maxTop: number = this.startTop + this.startHeight - this.minHeight;
      this.top = this.startTop + delta.y > 0 ? (this.startTop + delta.y < maxTop ? this.startTop + delta.y : maxTop) : 0;
    }

    if (attr.left === ApplyOperation.ADD) {
      const maxLeft: number = this.startLeft + this.startWidth - this.minWidth;
      this.left = this.startLeft + delta.x > 0 ? (this.startLeft + delta.x < maxLeft ? this.startLeft + delta.x : maxLeft) : 0;
    }
  }

  private rotateForm($event: MouseEvent | TouchEvent) {
    if (!this.rotating) {
      return;
    }

    const currentX: number = unifyEvent($event).clientX;
    const currentY: number = unifyEvent($event).clientY;

    this.rotate = Math.atan2(currentX - this.centerX, currentY - this.centerY) * (180 / Math.PI) * -1 + 180;
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

    this.parentWidth = this.el.parentElement.offsetWidth;
    this.parentHeight = this.el.parentElement.offsetHeight;

    this.centerX = this.el.offsetLeft + this.width / 2;
    this.centerY = this.el.offsetTop + this.height / 2;
  }

  private startMove() {
    if (this.dragBottomEnd || this.dragTopEnd || this.dragBottomStart || this.dragTopStart) {
      return;
    }

    if (this.dragTop || this.dragEnd || this.dragBottom || this.dragStart) {
      return;
    }

    if (this.rotating) {
      return;
    }

    this.moving = true;
  }

  private stop = () => {
    this.startX = null;
    this.startY = null;

    this.stopMove();
    this.stopResize();
    this.stopRotate();
  };

  private stopMove() {
    this.moving = false;

    this.startTop = null;
    this.startLeft = null;
  }

  private stopRotate() {
    this.rotating = false;

    this.centerX = null;
    this.centerY = null;
  }

  private stopResize() {
    this.dragBottomEnd = false;
    this.dragTopEnd = false;
    this.dragBottomStart = false;
    this.dragTopStart = false;

    this.dragTop = false;
    this.dragEnd = false;
    this.dragBottom = false;
    this.dragStart = false;

    this.startWidth = null;
    this.startHeight = null;
  }

  render() {
    return (
      <Host
        style={{
          '--width': `${this.width}px`,
          '--height': `${this.height}px`,
          '--top': `${this.top}px`,
          '--left': `${this.left}px`,
          '--rotate': this.rotate ? `${this.rotate}deg` : undefined
        }}
        class={`${this.selected ? 'selected' : ''} ${this.drag !== 'none' ? 'draggable' : ''} ${this.drag !== 'none' && this.moving ? 'drag' : ''}`}>
        {this.renderEdgesAnchors()}
        {this.renderBorderAnchors()}
        {this.renderRotateAnchor()}
        <slot />
      </Host>
    );
  }

  private renderEdgesAnchors() {
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

  private renderBorderAnchors() {
    if (!this.selected || !this.resize) {
      return undefined;
    }

    return [
      <div
        class="border top"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragTop = true)}
        onTouchStart={() => (this.dragTop = true)}>
        <div></div>
      </div>,
      <div
        class="border end"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragEnd = true)}
        onTouchStart={() => (this.dragEnd = true)}>
        <div></div>
      </div>,
      <div
        class="border bottom"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragBottom = true)}
        onTouchStart={() => (this.dragBottom = true)}>
        <div></div>
      </div>,
      <div
        class="border start"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragStart = true)}
        onTouchStart={() => (this.dragStart = true)}>
        <div></div>
      </div>
    ];
  }

  private renderRotateAnchor() {
    if (!this.selected || !this.rotation) {
      return undefined;
    }

    return (
      <div class="rotate">
        <div
          class="action"
          onClick={($event) => $event.stopPropagation()}
          onMouseDown={() => (this.rotating = true)}
          onTouchStart={() => (this.rotating = true)}>
          <div></div>
        </div>
        <div class="presentation"></div>
      </div>
    );
  }
}
