import { Component, h } from '@stencil/core';

@Component({
  tag: 'my-inline-action',
  //   styleUrl: 'deckdeckgo-inline-editor.scss',
  shadow: true
})
export class MyInlineAction {
  // @Element() el: HTMLElement;

  render() {
    return (
      <button
      // onClick={(e: UIEvent) => this.styleBold(e)}
      // disabled={this.disabledTitle}
      // class={this.bold ? 'bold active' : 'bold'}
      >
        B
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
