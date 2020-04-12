import {Component, h, Host, Prop} from '@stencil/core';

@Component({
  tag: 'deckgo-ie-separator',
  styleUrl: 'separator.scss',
  shadow: true
})
export class Separator {
  @Prop()
  mobile: boolean;

  render() {
    const cssClass = this.mobile ? 'deckgo-tools-mobile' : undefined;

    return (
      <Host class={cssClass}>
        <div class="separator"></div>
      </Host>
    );
  }
}
