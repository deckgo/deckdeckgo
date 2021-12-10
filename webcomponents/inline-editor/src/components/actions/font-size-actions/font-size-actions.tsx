import {Component, Event, EventEmitter, h, Host, Prop} from '@stencil/core';
import {FontSize} from '../../../types/enums';
import {ExecCommandAction} from '../../..';

@Component({
  tag: 'deckgo-ie-font-size-actions',
  styleUrl: 'font-size-actions.scss',
  shadow: true
})
export class FontSizeActions {
  @Prop()
  mobile: boolean;

  @Prop()
  sticky: boolean;

  @Prop()
  fontSize: FontSize;

  @Event()
  private execCommand: EventEmitter<ExecCommandAction>;

  private modifyFontSize($event: UIEvent, size: FontSize) {
    $event.stopPropagation();

    const value: string = Object.keys(FontSize).find((key) => FontSize[key] === size);

    this.execCommand.emit({
      cmd: 'style',
      detail: {
        style: 'font-size',
        value: value.toLowerCase().replace('_', '-'),
        initial: (element: HTMLElement | null) => element && element.style['font-size'] === value.toLowerCase().replace('_', '-')
      }
    });
  }

  render() {
    return (
      <Host class={this.sticky ? 'deckgo-tools-sticky' : undefined}>
        {this.renderAction(FontSize.X_SMALL)}
        {this.renderAction(FontSize.SMALL)}
        {this.renderAction(FontSize.MEDIUM)}
        {this.renderAction(FontSize.LARGE)}
        {this.renderAction(FontSize.X_LARGE)}
        {this.renderAction(FontSize.XX_LARGE)}
        {this.renderAction(FontSize.XXX_LARGE)}
      </Host>
    );
  }

  private renderAction(size: FontSize) {
    return (
      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.modifyFontSize($event.detail, size)}
        class={this.fontSize === size ? 'active' : undefined}>
        <span>{size.toString()}</span>
      </deckgo-ie-action-button>
    );
  }
}
