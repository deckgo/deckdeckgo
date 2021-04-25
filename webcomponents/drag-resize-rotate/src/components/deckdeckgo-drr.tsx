import {Component, h, Host, Prop, State, Element, Event, EventEmitter, Build} from '@stencil/core';

import {unifyEvent} from '@deckdeckgo/utils';

interface ResizeMatrix {
  a: 0 | 1;
  b: 0 | 1;
  c: 0 | 1;
  d: 0 | 1;
}

@Component({
  tag: 'deckgo-drr',
  styleUrl: 'deckdeckgo-drr.scss',
  shadow: true,
})
export class DeckdeckgoDragResizeRotate {
  @Element() el: HTMLElement;

  /**
   * The component could be use with percentage, viewport (vw/vh) or pixels (px) units. All relative to the container
   */
  @Prop()
  unit: 'percentage' | 'viewport' | 'px' = 'percentage';

  // Size

  /**
   * Allow or not the resize actions
   */
  @Prop({mutable: true})
  resize: boolean = true;

  // Position

  /**
   * Allow the component to be dragged in which direction
   */
  @Prop()
  drag: 'x-axis' | 'y-axis' | 'all' | 'none' = 'all';

  // Rotate

  /**
   * Allow or not the rotation of the element
   */
  @Prop()
  rotation: boolean = true;

  // Text content editable

  /**
   * To be used if your slotted element is to be defined as contentEditable. Useful for text edition. Note that if turns to true, the property resize is going to be set to false automatically
   */
  @Prop({reflect: true})
  text: boolean = false;

  // Size

  @State()
  private width: number;

  @State()
  private height: number;

  @State()
  private minWidth: number = 10;

  @State()
  private minHeight: number = 10;

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

  /**
   * Emitted when the component is selected or unselected. It propagates the host component itself
   */
  @Event()
  drrSelect: EventEmitter<HTMLElement | undefined>;

  /**
   * Emitted when the component is modified respectively when the user stop interacting. It propagates the host component itself
   */
  @Event()
  drrDidChange: EventEmitter<HTMLElement | undefined>;

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

  private qp0_x: number;
  private qp0_y: number;

  private pp_x: number;
  private pp_y: number;

  @State()
  private editing: boolean = false;

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
      document.removeEventListener('dblclick', this.dbclick, true);
    }
  }

  private async init() {
    this.width = this.el.style.getPropertyValue('--width') ? parseFloat(this.el.style.getPropertyValue('--width')) : 0;
    this.height = this.el.style.getPropertyValue('--height') ? parseFloat(this.el.style.getPropertyValue('--height')) : 0;
    this.top = this.el.style.getPropertyValue('--top') ? parseFloat(this.el.style.getPropertyValue('--top')) : 0;
    this.left = this.el.style.getPropertyValue('--left') ? parseFloat(this.el.style.getPropertyValue('--left')) : 0;
    this.rotate = this.el.style.getPropertyValue('--rotate') ? parseFloat(this.el.style.getPropertyValue('--rotate')) : 0;

    if (this.text) {
      this.resize = false;
    }
  }

  private async displaySlot() {
    const element: HTMLElement = this.el.querySelector(Build.isBrowser ? `:scope > *` : '> *');

    if (element && element.style.display === 'none') {
      element.style.display = '';
    }
  }

  private start = async ($event: MouseEvent | TouchEvent) => {
    if (!$event || !$event.target) {
      return;
    }

    // If we click elsewhere or select another component, then this component should loose focus and values need to be reset for next usage
    if (!this.isTargetEvent($event)) {
      this.stopAndReset(false);

      if (this.selected) {
        this.drrSelect.emit(undefined);
      }

      this.selected = false;

      this.resetTextEditable();

      return;
    }

    if (!this.editing) {
      const selected: HTMLElement = ($event.target as HTMLElement).closest('deckgo-drr');

      this.drrSelect.emit(selected);

      this.selected = true;

      this.initTextEditable();

      this.startMove();
    }

    await this.initStartPositions($event);
  };

  private isTargetEvent($event: MouseEvent | TouchEvent): boolean {
    const selected: HTMLElement = ($event.target as HTMLElement).closest('deckgo-drr');

    return selected && selected.isEqualNode(this.el);
  }

  private initTextEditable() {
    if (!this.text || !this.el) {
      return;
    }

    document.addEventListener('dblclick', this.dbclick.bind(this), {once: true});

    const element: HTMLElement = this.el.querySelector(Build.isBrowser ? `:scope > *` : '> *');
    if (element) {
      element.setAttribute('contentEditable', 'true');
    }
  }

  private resetTextEditable() {
    if (!this.text) {
      return;
    }

    this.editing = false;

    if (document) {
      document.removeEventListener('dblclick', this.dbclick, true);
    }
  }

  private async initStartPositions($event: MouseEvent | TouchEvent) {
    this.startX = unifyEvent($event).clientX;
    this.startY = unifyEvent($event).clientY;

    await this.initParentSize();

    this.initStartPositionsMove();
    this.initStartPositionsRotation();
    this.initStartPositionsResize();
  }

  private initStartPositionsMove() {
    this.startWidth = isNaN(this.width) ? 0 : this.width;
    this.startHeight = isNaN(this.height) ? 0 : this.height;

    this.startTop = this.top;
    this.startLeft = this.left;
  }

  private initStartPositionsRotation() {
    const rect: DOMRect = this.el.getBoundingClientRect();
    this.centerX = rect.left + rect.width / 2;
    this.centerY = rect.top + rect.height / 2;
  }

  private initStartPositionsResize() {
    const theta: number = (Math.PI * 2 * this.rotate) / 360;
    const cos_t: number = Math.cos(theta);
    const sin_t: number = Math.sin(theta);

    const css: CSSStyleDeclaration = window.getComputedStyle(this.el);

    const l: number = parseFloat(css.left);
    const t: number = parseFloat(css.top);
    const w: number = parseFloat(css.width);
    const h: number = parseFloat(css.height);

    const matrix: ResizeMatrix = this.resizeMatrix();

    const c0_x = l + w / 2.0;
    const c0_y = t + h / 2.0;

    const q0_x: number = l + matrix.a * w;
    const q0_y: number = t + matrix.b * h;

    const p0_x: number = l + matrix.c * w;
    const p0_y: number = t + matrix.d * h;

    this.qp0_x = q0_x * cos_t - q0_y * sin_t - c0_x * cos_t + c0_y * sin_t + c0_x;
    this.qp0_y = q0_x * sin_t + q0_y * cos_t - c0_x * sin_t - c0_y * cos_t + c0_y;

    this.pp_x = p0_x * cos_t - p0_y * sin_t - c0_x * cos_t + c0_y * sin_t + c0_x;
    this.pp_y = p0_x * sin_t + p0_y * cos_t - c0_x * sin_t - c0_y * cos_t + c0_y;
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
      this.deltaMove($event, false, true);
    } else if (this.drag === 'y-axis') {
      this.deltaMove($event, true, false);
    } else {
      this.deltaMove($event, true, true);
    }

    return true;
  }

  private size($event: MouseEvent | TouchEvent): boolean {
    if (!this.resize) {
      return false;
    }

    if (!this.selected || !this.startX || !this.startY) {
      return false;
    }

    if (
      !this.dragBottomEnd &&
      !this.dragTopEnd &&
      !this.dragBottomStart &&
      !this.dragTopStart &&
      !this.dragTop &&
      !this.dragEnd &&
      !this.dragBottom &&
      !this.dragStart
    ) {
      return false;
    }

    this.deltaResize($event);

    return true;
  }

  private deltaMove($event: MouseEvent | TouchEvent, top: boolean, left: boolean) {
    const delta: {x: number; y: number} = this.getDelta($event);

    const deltaX: number = this.convertToUnit(delta.x, 'width');
    const deltaY: number = this.convertToUnit(delta.y, 'height');

    if (top) {
      const maxTop: number = this.convertParentUnit(this.parentHeight) - this.startHeight;
      this.top = this.startTop + deltaY > 0 ? (this.startTop + deltaY < maxTop ? this.startTop + deltaY : maxTop) : 0;
    }

    if (left) {
      const maxLeft: number = this.convertParentUnit(this.parentWidth) - this.startWidth;
      this.left = this.startLeft + deltaX > 0 ? (this.startLeft + deltaX < maxLeft ? this.startLeft + deltaX : maxLeft) : 0;
    }
  }

  private deltaResize($event: MouseEvent | TouchEvent) {
    const delta: {x: number; y: number} = this.getDelta($event);

    const qp_x: number = this.qp0_x + delta.x;
    const qp_y: number = this.qp0_y + delta.y;

    const cp_x: number = (qp_x + this.pp_x) / 2.0;
    const cp_y: number = (qp_y + this.pp_y) / 2.0;

    const mtheta: number = (-1 * Math.PI * 2 * this.rotate) / 360;
    const cos_mt: number = Math.cos(mtheta);
    const sin_mt: number = Math.sin(mtheta);

    let q_x: number = qp_x * cos_mt - qp_y * sin_mt - cos_mt * cp_x + sin_mt * cp_y + cp_x;
    let q_y: number = qp_x * sin_mt + qp_y * cos_mt - sin_mt * cp_x - cos_mt * cp_y + cp_y;

    let p_x: number = this.pp_x * cos_mt - this.pp_y * sin_mt - cos_mt * cp_x + sin_mt * cp_y + cp_x;
    let p_y: number = this.pp_x * sin_mt + this.pp_y * cos_mt - sin_mt * cp_x - cos_mt * cp_y + cp_y;

    const matrix: ResizeMatrix = this.resizeMatrix();

    const wtmp: number = matrix.a * (q_x - p_x) + matrix.c * (p_x - q_x);
    const htmp: number = matrix.b * (q_y - p_y) + matrix.d * (p_y - q_y);

    let w: number;
    let h: number;

    if (wtmp < this.minWidth || htmp < this.minHeight) {
      w = Math.max(this.minWidth, wtmp);
      h = Math.max(this.minHeight, htmp);

      const theta: number = -1 * mtheta;
      const cos_t: number = Math.cos(theta);
      const sin_t: number = Math.sin(theta);

      const dh_x: number = -sin_t * h;
      const dh_y: number = cos_t * h;

      const dw_x: number = cos_t * w;
      const dw_y: number = sin_t * w;

      const qp_x_min: number = this.pp_x + (matrix.a - matrix.c) * dw_x + (matrix.b - matrix.d) * dh_x;
      const qp_y_min: number = this.pp_y + (matrix.a - matrix.c) * dw_y + (matrix.b - matrix.d) * dh_y;

      const cp_x_min: number = (qp_x_min + this.pp_x) / 2.0;
      const cp_y_min: number = (qp_y_min + this.pp_y) / 2.0;

      q_x = qp_x_min * cos_mt - qp_y_min * sin_mt - cos_mt * cp_x_min + sin_mt * cp_y_min + cp_x_min;
      q_y = qp_x_min * sin_mt + qp_y_min * cos_mt - sin_mt * cp_x_min - cos_mt * cp_y_min + cp_y_min;

      p_x = this.pp_x * cos_mt - this.pp_y * sin_mt - cos_mt * cp_x_min + sin_mt * cp_y_min + cp_x_min;
      p_y = this.pp_x * sin_mt + this.pp_y * cos_mt - sin_mt * cp_x_min - cos_mt * cp_y_min + cp_y_min;
    } else {
      w = wtmp;
      h = htmp;
    }

    const l: number = matrix.c * q_x + matrix.a * p_x;
    const t: number = matrix.d * q_y + matrix.b * p_y;

    this.left = this.convertToUnit(l, 'width');
    this.width = this.convertToUnit(w, 'width');

    this.top = this.convertToUnit(t, 'height');
    this.height = this.convertToUnit(h, 'height');
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

    return {
      x: this.dragBottom || this.dragTop ? 0 : currentX - this.startX,
      y: this.dragStart || this.dragEnd ? 0 : currentY - this.startY,
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

  private resizeMatrix(): ResizeMatrix {
    const a: 0 | 1 = this.dragBottomEnd || this.dragTopEnd || this.dragEnd ? 1 : 0;
    const b: 0 | 1 = this.dragBottomEnd || this.dragBottomStart || this.dragStart || this.dragBottom ? 1 : 0;
    const c: 0 | 1 = a === 1 ? 0 : 1;
    const d: 0 | 1 = b === 1 ? 0 : 1;

    return {
      a,
      b,
      c,
      d,
    };
  }

  private dbclick = async ($event: MouseEvent | TouchEvent) => {
    if (!this.isTargetEvent($event)) {
      return;
    }

    this.editing = true;

    const element: HTMLElement = this.el.querySelector(Build.isBrowser ? `:scope > *` : '> *');
    if (element) {
      element.focus();

      await this.moveCursorToEnd(element);
    }
  };

  // https://stackoverflow.com/a/3866442/5404186
  private async moveCursorToEnd(contentEditableElement: HTMLElement) {
    if (window && document && document.createRange && contentEditableElement) {
      const range: Range = document.createRange();
      range.selectNodeContents(contentEditableElement);
      range.collapse(false);

      const selection: Selection = window.getSelection();
      selection.removeAllRanges();
      selection.addRange(range);
    }
  }

  render() {
    const widthUnit: string = this.unit === 'percentage' ? '%' : this.unit === 'viewport' ? 'vw' : this.unit;
    const heightUnit: string = this.unit === 'percentage' ? '%' : this.unit === 'viewport' ? 'vh' : this.unit;

    return (
      <Host
        style={{
          '--width': this.text ? 'auto' : `${this.width}${widthUnit}`,
          '--height': this.text ? 'auto' : `${this.height}${heightUnit}`,
          '--top': `${this.top}${heightUnit}`,
          '--left': `${this.left}${widthUnit}`,
          '--rotate': this.rotate ? `${this.rotate}deg` : `0deg`,
          '--pointer-events': `${this.editing ? 'all' : 'none'}`,
          '--user-select': `${this.text ? 'text' : 'none'}`,
        }}
        class={`${this.selected ? 'selected' : ''} ${this.text ? 'text' : ''} ${this.drag !== 'none' ? 'draggable' : ''} ${
          this.drag !== 'none' && this.moving ? 'drag' : ''
        }`}>
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
      </div>,
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
        onTouchStart={() => (this.dragStart = true)}></div>,
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
