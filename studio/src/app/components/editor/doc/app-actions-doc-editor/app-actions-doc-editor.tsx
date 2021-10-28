import {Component, Listen, h} from '@stencil/core';

@Component({
  tag: 'app-actions-doc-editor',
  styleUrl: 'app-actions-doc-editor.scss',
  shadow: false
})
export class AppActionsDocEditor {
  @Listen('keydown', {target: 'document', passive: true})
  onKeyDown($event: KeyboardEvent) {
    console.log($event);
  }

  @Listen('mousedown', {target: 'document', passive: true})
  onMouseDown($event: MouseEvent) {
    console.log($event);
  }

  @Listen('touchstart', {target: 'document', passive: true})
  onTouchStart($event: TouchEvent) {
    console.log($event);
  }

  private test() {
    // TODO: catch clean paste events
    // TODO: export
    // TODO: tab
    // TODO: refactor article

    document.execCommand('insertHTML', false, '<span id="myId">hi</span>');
  }

  render() {
    return <ion-button onClick={() => this.test()}>Test</ion-button>;
  }
}
