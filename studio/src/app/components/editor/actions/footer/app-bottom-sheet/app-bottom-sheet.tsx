import {Component, h, Element, Host, State, Event, EventEmitter} from '@stencil/core';

import {debounce, unifyEvent} from '@deckdeckgo/utils';

@Component({
  tag: 'app-bottom-sheet',
  styleUrl: 'app-bottom-sheet.scss',
})
export class AppBottomSheet {
  @Element() el: HTMLElement;

  private startY: number;

  private readonly bottomSheetMinHeight: number = 32;

  private heightOffset: number;

  @State()
  private contentHeight: number;

  @State()
  private bottomSheetTop: number = this.bottomSheetMinHeight;

  private container: HTMLElement;
  private content: HTMLElement;

  @Event() sheetChanged: EventEmitter<'open' | 'close'>;

  async componentDidLoad() {
    await this.initSize();
    await this.init();

    this.initWindowResize();
  }

  async disconnectedCallback() {
    await this.destroy();

    this.removeWindowResize();
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onWindowResize));
    }
  }

  private removeWindowResize() {
    if (window) {
      window.removeEventListener('resize', debounce(this.onWindowResize));
    }
  }

  private onWindowResize = async () => {
    await this.initSize();
  };

  private initSize(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.bottomSheetTop = this.bottomSheetMinHeight;
      this.heightOffset = this.content.offsetHeight || this.el.offsetHeight;
      this.contentHeight = window.innerHeight || screen.height;

      resolve();
    });
  }

  private init(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.container) {
        resolve();
        return;
      }

      this.container.addEventListener('mousedown', this.startEvent, {passive: false});
      this.container.addEventListener('touchstart', this.startEvent, {passive: false});
      document.addEventListener('mouseup', this.endEvent, {passive: false});
      document.addEventListener('touchend', this.endEvent, {passive: false});

      resolve();
    });
  }

  private destroy(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.container) {
        resolve();
        return;
      }

      this.container.removeEventListener('mousedown', this.startEvent, true);
      this.container.removeEventListener('touchstart', this.startEvent, true);
      document.removeEventListener('mouseup', this.endEvent, true);
      document.removeEventListener('touchend', this.endEvent, true);

      resolve();
    });
  }

  private startEvent = ($event: MouseEvent | TouchEvent) => {
    $event.preventDefault();

    this.startY = unifyEvent($event).clientY;
  };

  private endEvent = ($event: MouseEvent | TouchEvent) => {
    if (!this.startY || this.startY === undefined) {
      return;
    }

    $event.preventDefault();

    const toY: number = unifyEvent($event).clientY;

    this.toggleBottomSheet(toY);
  };

  private toggleBottomSheet(toY: number) {
    if (this.startY === toY) {
      // It's a click
      this.startY = undefined;
      return;
    }

    if (this.startY > toY) {
      this.bottomSheetTop =
        this.bottomSheetTop <= this.bottomSheetMinHeight
          ? this.heightOffset
          : this.bottomSheetTop + this.heightOffset >= this.content.offsetHeight
          ? this.content.offsetHeight
          : this.bottomSheetTop + this.heightOffset;
    } else {
      this.bottomSheetTop = this.bottomSheetMinHeight;
    }

    this.startY = undefined;

    this.sheetChanged.emit(this.bottomSheetTop === this.bottomSheetMinHeight ? 'close' : 'open');
  }

  render() {
    return (
      <Host
        style={{
          '--bottom-sheet-top': `${this.bottomSheetTop}px`,
          '--contentheight': `${this.contentHeight}px`,
        }}>
        <div class="container" ref={(el) => (this.container = el)}>
          <div class="sheet-indicator"></div>
          <div class="content" ref={(el) => (this.content = el)}>
            <slot></slot>
          </div>
        </div>
      </Host>
    );
  }
}
