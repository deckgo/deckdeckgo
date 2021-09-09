import {Component, h, Prop, Host} from '@stencil/core';

@Component({
  tag: 'deckgo-ie-action-image',
  styleUrl: 'action-image.scss',
  shadow: true
})
export class ActionImage {
  @Prop()
  cssClass: string;

  render() {
    return (
      <Host class={this.cssClass}>
        <div></div>
      </Host>
    );
  }
}
