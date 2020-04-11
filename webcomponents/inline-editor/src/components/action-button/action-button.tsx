import {Component, h, Event, EventEmitter, Prop, Host} from '@stencil/core';

@Component({
  tag: 'deckgo-ie-action-button',
  styleUrl: 'action-button.scss',
  shadow: true
})
export class ActionButton {
  @Prop()
  disableAction: boolean = false;

  @Prop()
  cssClass: string;

  @Prop()
  mobile: boolean;

  @Event()
  action: EventEmitter<UIEvent>;

  render() {
    const cssClass = this.mobile ? 'deckgo-tools-mobile' : undefined;

    return (
      <Host class={cssClass}>
        <button onClick={($event: UIEvent) => this.action.emit($event)} disabled={this.disableAction} class={this.cssClass}>
          <slot></slot>
        </button>
      </Host>
    );
  }
}
