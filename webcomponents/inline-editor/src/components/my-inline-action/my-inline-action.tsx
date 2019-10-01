// Overall advantage over @Event() based solution:
// - if we use events to send info to the custom action it must be ABOVE the deckdeckgo-inline-editor to be able to receive it
// => we would need to care where the component is placed in the DOM
// With offering the execCommand listener we're more free here

import { Component, Event, EventEmitter, h, Prop, Watch } from '@stencil/core';

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
  @Prop()
  selection: Selection;
  @Watch('selection')
  protected selectionHandler() {
    // We can react on changes to selection and always have the latest available
    console.log('my-inline-action, selection changed');
  }
  @Event()
  commandTriggered: EventEmitter<InlineAction>;
  // INTERFACE STARTS HERE

  private async onCustomAction(e: UIEvent): Promise<void> {
    console.log('styleCustomAction', e.detail);
    e.stopPropagation();

    // BAD:
    // - we don't use the selectioj.toString / text.length check which is nice and you implemented already
    // we need to call the full API includine the 2nd parameter "showUI" which we don't want
    // - still if someone wnats do it it that way he can
    // await document.execCommand(
    //   'insertHTML',
    //   false,
    //   `<acme-placeholder class="own-class">${this.selection}</acme-placeholder>`
    // );

    this.commandTriggered.emit({
      command: 'insertHTML',
      value: `<acme-placeholder class="own-class">${this.selection}</acme-placeholder>`
    });

    // await this.initStyle(this.selection);
  }

  render() {
    return (
      <button
        onClick={(e: UIEvent) => this.onCustomAction(e)}
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
