import { Component, Event, EventEmitter, h } from '@stencil/core';

import {
  InlineActionComponent,
  InlineAction
} from '../inline-editor/deckdeckgo-inline-editor.interface';

@Component({
  tag: 'my-inline-action',
  styleUrl: 'my-inline-action.scss',
  shadow: true
})
export class MyInlineAction implements InlineActionComponent {
  // INTERFACE STARTS HERE
  @Event()
  actionTriggered: EventEmitter<InlineAction>;
  // INTERFACE STARTS HERE

  private onButtonClicked(e: UIEvent) {
    console.log('onButtonClicked', e);
    e.stopPropagation();

    this.actionTriggered.emit({
      command: 'insertHTML',
      value:
        "<acme-placeholder class='own-class'>" +
        document.getSelection() +
        '</acme-placeholder>'
    });
  }

  render() {
    return (
      <button
        onClick={(e: UIEvent) => this.onButtonClicked(e)}
        // disabled={this.disabledTitle}
        // class={this.bold ? 'bold active' : 'bold'}
      >
        C
      </button>
    );
  }

  hostData() {
    return {
      class: {
        //   'deckgo-tools-ios': isIOS()
      }
    };
  }
}
