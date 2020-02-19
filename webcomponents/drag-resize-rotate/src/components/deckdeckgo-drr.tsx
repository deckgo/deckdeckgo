import {Component, h, Host, Prop, State, Element, Event, EventEmitter} from '@stencil/core';

import {unifyEvent} from '@deckdeckgo/utils';

enum ApplyOperation {
  ADD,
  SUBSTRACT
}

@Component({
  tag: 'deckgo-drr',
  styleUrl: 'deckdeckgo-drr.scss',
  shadow: true
})
export class DeckdeckgoDragResizeRotate {
  @Element() el: HTMLElement;

  @Prop()
  unit: 'percentage' | 'viewport' | 'px' = 'percentage';

  // Size

  @Prop()
  resize: boolean = true;

  // Position

  @Prop()
  drag: 'x-axis' | 'y-axis' | 'all' | 'none' = 'all';

  // Rotate

  @Prop()
  rotation: boolean = true;

  // Size

  @State()
  private width: number;

  @State()
  private height: number;

  @State()
  private minWidth: number = 5;

  @State()
  private minHeight: number = 5;

  // Position

  @State()
  private top: number;

  @State()
  private left: number;

  // Rotate

  @State()
  private rotate: number;

  @State()
  private selected: boolean = false;

  private updated: boolean = false;

  @State()
  private moving: boolean = false;

  @Event()
  private drrSelect: EventEmitter<HTMLElement | undefined>;

  @Event()
  private drrDidChange: EventEmitter<HTMLElement | undefined>;

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

  async componentWillLoad() {
    await this.init();

    await this.attachEventListeners();
  }

  async componentDidLoad() {
    await this.displaySlot();
  }

  async componentWillUnload() {
    if (this.drag !== 'none' || this.resize) {
      await this.detachEventListeners();
    }
  }

  private async attachEventListeners() {
    const editable: boolean = this.el && this.el.parentElement && !this.el.parentElement.classList.contains('deckgo-read-only');

    if (editable && document && (this.drag !== 'none' || this.resize)) {
      document.addEventListener('mousedown', this.start.bind(this), {passive: true});
      document.addEventListener('touchstart', this.start.bind(this), {passive: true});
      document.addEventListener('mousemove', this.transform.bind(this), {passive: true});
      document.addEventListener('touchmove', this.transform.bind(this), {passive: true});
      document.addEventListener('mouseup', this.stop.bind(this), {passive: true});
      document.addEventListener('touchend', this.stop.bind(this), {passive: true});
    }
  }

  private async detachEventListeners() {
    if (document) {
      document.removeEventListener('mousedown', this.start, true);
      document.removeEventListener('touchstart', this.start, true);
      document.removeEventListener('mousemove', this.transform, true);
      document.removeEventListener('touchmove', this.transform, true);
      document.removeEventListener('mouseup', this.stop, true);
      document.removeEventListener('touchend', this.stop, true);
    }
  }

  private init(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.width = this.el.style.getPropertyValue('--width') ? parseFloat(this.el.style.getPropertyValue('--width')) : 0;
      this.height = this.el.style.getPropertyValue('--height') ? parseFloat(this.el.style.getPropertyValue('--height')) : 0;
      this.top = this.el.style.getPropertyValue('--top') ? parseFloat(this.el.style.getPropertyValue('--top')) : 0;
      this.left = this.el.style.getPropertyValue('--left') ? parseFloat(this.el.style.getPropertyValue('--left')) : 0;
      this.rotate = this.el.style.getPropertyValue('--rotate') ? parseFloat(this.el.style.getPropertyValue('--rotate')) : 0;

      this.minWidth = this.el.style.getPropertyValue('--min-width') ? parseFloat(this.el.style.getPropertyValue('--min-width')) : 5;
      this.minHeight = this.el.style.getPropertyValue('--min-height') ? parseFloat(this.el.style.getPropertyValue('--min-height')) : 5;

      resolve();
    });
  }

  private async displaySlot() {
    const element: HTMLElement = this.el.querySelector(`:scope > *`);

    if (element && element.style.display === 'none') {
      element.style.display = '';
    }
  }

  private start = async ($event: MouseEvent | TouchEvent) => {
    if (!$event || !$event.target) {
      return;
    }

    const selected: HTMLElement = ($event.target as HTMLElement).closest('deckgo-drr');

    // If we click elsewhere or select another component, then this component should loose focus and values need to be reset for next usage
    if (!selected || !selected.isEqualNode(this.el)) {
      this.stopAndReset(false);

      if (this.selected) {
        this.drrSelect.emit(undefined);
      }

      this.selected = false;
      return;
    }

    this.drrSelect.emit(selected);

    this.selected = true;

    this.startMove();
    await this.initStartPositions($event);
  };

  private transform = ($event: MouseEvent | TouchEvent) => {
    const moved: boolean = this.move($event);
    const resized: boolean = this.size($event);
    const rotated: boolean = this.rotateShape($event);

    this.updated = moved || resized || rotated;
  };

  private move($event: MouseEvent | TouchEvent): boolean {
    if (!this.moving || this.drag === 'none') {
      return false;
    }

    if (this.drag === 'x-axis') {
      this.deltaMove($event, {left: ApplyOperation.ADD});
    } else if (this.drag === 'y-axis') {
      this.deltaMove($event, {top: ApplyOperation.ADD});
    } else {
      this.deltaMove($event, {top: ApplyOperation.ADD, left: ApplyOperation.ADD});
    }

    return true;
  }

  private size($event: MouseEvent | TouchEvent): boolean {
    if (!this.resize) {
      return false;
    }

    if (!this.selected || !this.startX || !this.startY || !this.startWidth || !this.startHeight) {
      return false;
    }

    if (this.dragBottomEnd) {
      this.deltaResize($event, {width: ApplyOperation.ADD, height: ApplyOperation.ADD});
    } else if (this.dragTopEnd) {
      this.deltaResize($event, {width: ApplyOperation.ADD, height: ApplyOperation.SUBSTRACT, top: ApplyOperation.ADD});
    } else if (this.dragBottomStart) {
      this.deltaResize($event, {width: ApplyOperation.SUBSTRACT, height: ApplyOperation.ADD, left: ApplyOperation.ADD});
    } else if (this.dragTopStart) {
      this.deltaResize($event, {
        width: ApplyOperation.SUBSTRACT,
        top: ApplyOperation.ADD,
        height: ApplyOperation.SUBSTRACT,
        left: ApplyOperation.ADD
      });
    } else if (this.dragTop) {
      this.deltaResize($event, {top: ApplyOperation.ADD, height: ApplyOperation.SUBSTRACT});
    } else if (this.dragEnd) {
      this.deltaResize($event, {width: ApplyOperation.ADD});
    } else if (this.dragBottom) {
      this.deltaResize($event, {height: ApplyOperation.ADD});
    } else if (this.dragStart) {
      this.deltaResize($event, {left: ApplyOperation.ADD, width: ApplyOperation.SUBSTRACT});
    }

    return true;
  }

  private deltaMove($event: MouseEvent | TouchEvent, attr: {top?: ApplyOperation; left?: ApplyOperation}) {
    const delta: {x: number; y: number} = this.getDelta($event);

    if (attr.top === ApplyOperation.ADD) {
      const maxTop: number = this.convertParentUnit(this.parentHeight) - this.startHeight;
      this.top = this.startTop + delta.y > 0 ? (this.startTop + delta.y < maxTop ? this.startTop + delta.y : maxTop) : 0;
    }

    if (attr.left === ApplyOperation.ADD) {
      const maxLeft: number = this.convertParentUnit(this.parentWidth) - this.startWidth;
      this.left = this.startLeft + delta.x > 0 ? (this.startLeft + delta.x < maxLeft ? this.startLeft + delta.x : maxLeft) : 0;
    }
  }

  private deltaResize($event: MouseEvent | TouchEvent, attr: {width?: ApplyOperation; height?: ApplyOperation; top?: ApplyOperation; left?: ApplyOperation}) {
    const delta: {x: number; y: number} = this.getDelta($event);

    if (attr.width === ApplyOperation.ADD) {
      const maxWidth: number = this.convertParentUnit(this.parentWidth) - this.startLeft;
      this.width = this.startWidth + delta.x > this.minWidth ? (this.startWidth + delta.x < maxWidth ? this.startWidth + delta.x : maxWidth) : this.minWidth;
    } else if (attr.width === ApplyOperation.SUBSTRACT) {
      const maxWidth: number = this.startLeft + this.startWidth;
      this.width = this.startWidth - delta.x > this.minWidth ? (this.startWidth - delta.x < maxWidth ? this.startWidth - delta.x : maxWidth) : this.minWidth;
    }

    if (attr.height === ApplyOperation.ADD) {
      const maxHeight: number = this.convertParentUnit(this.parentHeight) - this.startTop;
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

    // TODO: Resize stick corner
    // const currentX: number = unifyEvent($event).clientX;
    // const currentY: number = unifyEvent($event).clientY;
    //
    // const phi: number = this.rotate !== undefined ? (this.rotate * Math.PI) / 180 : 0;
    //
    // const a = currentX;
    // const b = -2 * (Math.cos(phi) * Math.sin(phi) * currentY);
    // const c = -1 * Math.cos(phi) * this.width;
    // const d = -1 * Math.sin(phi) * this.height;
    //
    // this.left = a + b + c + d;
    //
    // const e = 2 * (Math.cos(phi) * Math.sin(phi) * currentX);
    // const f = currentY;
    // const g = -1  * Math.cos(phi) * this.height;
    // const h = -1 * Math.sin(phi) * this.width;
    //
    // this.top = -1 * (e + f + g + h);
  }

  private rotateShape($event: MouseEvent | TouchEvent): boolean {
    if (!this.rotating) {
      return false;
    }

    const currentX: number = unifyEvent($event).clientX;
    const currentY: number = unifyEvent($event).clientY;

    this.rotate = Math.atan2(currentX - this.centerX, currentY - this.centerY) * (180 / Math.PI) * -1 + 180;

    return true;
  }

  private getDelta($event: MouseEvent | TouchEvent): {x: number; y: number} {
    const currentX: number = unifyEvent($event).clientX;
    const currentY: number = unifyEvent($event).clientY;

    const deltaX: number = this.convertToUnit(currentX - this.startX, 'width');
    const deltaY: number = this.convertToUnit(currentY - this.startY, 'height');

    return {
      x: deltaX,
      y: deltaY
    };
  }

  private convertToUnit(value: number, ratio: 'width' | 'height'): number {
    if (this.unit === 'px') {
      return value;
    }

    if (this.unit === 'viewport') {
      const windowSize: number = ratio === 'width' ? window.innerWidth || screen.width : window.innerHeight || screen.height;
      return (value * 100) / windowSize;
    }

    const parentSize: number = ratio === 'width' ? this.parentWidth : this.parentHeight;
    return (value * 100) / parentSize;
  }

  private convertParentUnit(value: number): number {
    return this.unit === 'percentage' ? 100 : value;
  }

  private async initStartPositions($event: MouseEvent | TouchEvent) {
    this.startX = unifyEvent($event).clientX;
    this.startY = unifyEvent($event).clientY;

    this.startWidth = this.width;
    this.startHeight = this.height;

    this.startTop = this.top;
    this.startLeft = this.left;

    await this.initParentSize();

    this.centerX = this.el.getBoundingClientRect().left + this.el.offsetWidth / 2;
    this.centerY = this.el.getBoundingClientRect().top + this.el.offsetHeight / 2;
  }

  private async initParentSize() {
    // The deckgo-slide-aspect-ratio template exposes a getContainer function which return a reference to the effective container.
    if (this.el.parentElement && typeof (this.el.parentElement as any).getContainer === 'function') {
      const parent: HTMLElement = await (this.el.parentElement as any).getContainer();

      this.parentWidth = this.unit === 'percentage' ? parent.offsetWidth : this.convertToUnit(parent.offsetWidth, 'width');
      this.parentHeight = this.unit === 'percentage' ? parent.offsetHeight : this.convertToUnit(parent.offsetHeight, 'height');
    } else {
      this.parentWidth = this.unit === 'percentage' ? this.el.parentElement.offsetWidth : this.convertToUnit(this.el.parentElement.offsetWidth, 'width');
      this.parentHeight = this.unit === 'percentage' ? this.el.parentElement.offsetHeight : this.convertToUnit(this.el.parentElement.offsetHeight, 'height');
    }
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
    this.stopAndReset(this.selected);
  };

  private stopAndReset(emitUpdate: boolean) {
    this.startX = null;
    this.startY = null;

    this.stopMove();
    this.stopResize();
    this.stopRotate();

    if (emitUpdate && this.updated) {
      this.drrDidChange.emit(this.el.shadowRoot.host as HTMLElement);
    }

    this.updated = false;
  }

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
    const widthUnit: string = this.unit === 'percentage' ? '%' : this.unit === 'viewport' ? 'vw' : this.unit;
    const heightUnit: string = this.unit === 'percentage' ? '%' : this.unit === 'viewport' ? 'vh' : this.unit;

    return (
      <Host
        style={{
          '--width': `${this.width}${widthUnit}`,
          '--height': `${this.height}${heightUnit}`,
          '--top': `${this.top}${heightUnit}`,
          '--left': `${this.left}${widthUnit}`,
          '--rotate': this.rotate ? `${this.rotate}deg` : `0deg`
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
        onTouchStart={() => (this.dragTop = true)}></div>,
      <div
        class="border end"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragEnd = true)}
        onTouchStart={() => (this.dragEnd = true)}></div>,
      <div
        class="border bottom"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragBottom = true)}
        onTouchStart={() => (this.dragBottom = true)}></div>,
      <div
        class="border start"
        onClick={($event) => $event.stopPropagation()}
        onMouseDown={() => (this.dragStart = true)}
        onTouchStart={() => (this.dragStart = true)}></div>
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
