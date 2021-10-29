import {Component, Listen, h} from '@stencil/core';

@Component({
  tag: 'app-actions-doc-editor',
  styleUrl: 'app-actions-doc-editor.scss',
  shadow: false
})
export class AppActionsDocEditor {
  @Listen('keydown', {target: 'document', passive: true})
  onKeyDown(_$event: KeyboardEvent) {}

  @Listen('mousedown', {target: 'document', passive: true})
  onMouseDown(_$event: MouseEvent) {}

  @Listen('touchstart', {target: 'document', passive: true})
  onTouchStart(_$event: TouchEvent) {}

  private test() {
    // TODO: reset deck and doc store on page change
    // TODO: this component and add ddg cmp
    // TODO: color bug
    // TODO: redo popover

    document.execCommand('insertHTML', false, '<span id="myId">hi</span>');
  }

  render() {
    return <ion-button onClick={() => this.test()}>Test</ion-button>;
  }
}
