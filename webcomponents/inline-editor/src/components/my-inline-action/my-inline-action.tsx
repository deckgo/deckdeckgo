import { Component, h } from '@stencil/core';

@Component({
  tag: 'my-inline-action',
  styleUrl: 'my-inline-action.scss',
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
        C
      </button>
      //   <h5>Custom</h5>
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
