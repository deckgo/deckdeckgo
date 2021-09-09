import {Component, h, Element, Host, State} from '@stencil/core';

import {debounce, unifyEvent} from '@deckdeckgo/utils';

@Component({
  tag: 'app-bottom-sheet',
  styleUrl: 'app-bottom-sheet.scss'
})
export class AppBottomSheet {
  @Element() el: HTMLElement;

  private startY: number;

  private bottomSheetMinHeight: number = 48;
  private heightOffset: number;

  @State()
  private contentHeight: number;

  @State()
  private toolbarOffset: number = 56;

  @State()
  private bottomSheetTop: number = this.bottomSheetMinHeight;

  private container: HTMLElement;
  private content: HTMLElement;

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
      this.heightOffset = (window.innerHeight || screen.height) * 0.8;
      this.contentHeight = window.innerHeight || screen.height;

      const header: HTMLElement = document.querySelector('ion-nav app-remote ion-header');

      if (header) {
        this.toolbarOffset = header.offsetHeight;
      }

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

  private endEvent = ($event: MouseEvent) => {
    if (!this.startY || this.startY === undefined) {
      return;
    }

    $event.preventDefault();

    const toY: number = unifyEvent($event).clientY;

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
  };

  render() {
    return (
      <Host
        style={{
          '--bottom-sheet-top': `${this.bottomSheetTop}px`,
          '--bottom-sheet-toolbaroffset': `${this.toolbarOffset}px`,
          '--contentheight': `${this.contentHeight}px`
        }}>
        {this.renderBackdrop()}
        <div class="container" ref={(el) => (this.container = el)}>
          <div class="indicator"></div>
          <div class="content ion-padding-top" ref={(el) => (this.content = el)}>
            <slot></slot>
          </div>
        </div>
      </Host>
    );
  }

  private renderBackdrop() {
    if (this.bottomSheetTop > this.bottomSheetMinHeight) {
      return <ion-backdrop></ion-backdrop>;
    } else {
      return undefined;
    }
  }
}
