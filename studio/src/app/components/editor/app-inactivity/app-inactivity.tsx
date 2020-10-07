import {Component, EventEmitter, Event, Prop, Watch} from '@stencil/core';

@Component({
  tag: 'app-inactivity',
  shadow: true,
})
export class AppInactivity {
  @Prop()
  fullscreen: boolean = false;

  private cursorHidden: boolean = false;
  private idleMouseTimer: NodeJS.Timeout;

  @Event() private mouseInactivity: EventEmitter<boolean>;

  @Watch('fullscreen')
  async onFullscreenToggle(newValue: boolean, oldValue: boolean) {
    if (oldValue) {
      await this.removeEventListener();
    } else {
      await this.addEventListener();
    }

    if (!oldValue && newValue) {
      setTimeout(() => {
        this.hideEditing(false);
      }, 400);
    }
  }

  async disconnectedCallback() {
    await this.removeEventListener();
  }

  private async addEventListener() {
    if (!window) {
      return;
    }

    window.addEventListener('keypress', this.clearMouseCursorTimer, {passive: true});
    window.addEventListener('mousemove', this.clearMouseCursorTimer, {passive: true});
    window.addEventListener('touchmove', this.clearMouseCursorTimer, {passive: true});
  }

  private async removeEventListener() {
    if (!window) {
      return;
    }

    window.removeEventListener('keypress', this.clearMouseCursorTimer, true);
    window.removeEventListener('mousemove', this.clearMouseCursorTimer, true);
    window.removeEventListener('touchmove', this.clearMouseCursorTimer, true);
  }

  private clearMouseCursorTimer = async () => {
    if (this.idleMouseTimer) {
      clearTimeout(this.idleMouseTimer);
    }

    await this.showHideMouseCursor(true);

    this.hideEditing(true);
  };

  private hideEditing(withDelay: boolean) {
    this.idleMouseTimer = setTimeout(
      async () => {
        await this.showHideMouseCursor(false);
      },
      withDelay ? 2000 : 0
    );
  }

  private showHideMouseCursor(show: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.cursorHidden && show) {
        // Cursor already displayed, we don't want to touch the style multiple times if not needed
        resolve();
        return;
      }

      this.mouseInactivity.emit(show);

      this.cursorHidden = !show;

      resolve();
    });
  }
}
