import {Component, EventEmitter, h, Prop, Event, Host} from '@stencil/core';

import {ContentAlign} from '../../../types/enums';

import { execCommandAlign } from "../../../utils/execcommand-align.utils";
import { execCommandNativeAlign } from "../../../utils/execcommnad-native.utils";

@Component({
  tag: 'deckgo-ie-align-actions',
  styleUrl: 'align-actions.scss',
  shadow: true,
})
export class AlignActions {
  @Prop()
  anchorEvent: MouseEvent | TouchEvent;

  @Prop()
  mobile: boolean;

  @Prop()
  sticky: boolean;

  @Prop()
  contentAlign: ContentAlign;

  @Prop()
  containers: string;

  @Prop()
  command: 'native' | 'custom' = 'native';

  @Event()
  private alignModified: EventEmitter;

  private justifyContent($event: UIEvent, align: ContentAlign): Promise<void> {
    return new Promise<void>(async (resolve) => {
      $event.stopPropagation();

      if (this.command === 'native') {
        execCommandNativeAlign(align);
      } else {
        await execCommandAlign(this.anchorEvent, this.containers, align);
      }

      await this.alignModified.emit();

      resolve();
    });
  }

  render() {
    return (
      <Host class={this.sticky ? 'deckgo-tools-sticky' : undefined}>
        <deckgo-ie-action-button
          mobile={this.mobile}
          onAction={($event: CustomEvent<UIEvent>) => this.justifyContent($event.detail, ContentAlign.LEFT)}
          class={this.contentAlign === ContentAlign.LEFT ? 'active' : undefined}>
          <deckgo-ie-action-image cssClass={'left-align'}></deckgo-ie-action-image>
        </deckgo-ie-action-button>
        <deckgo-ie-action-button
          mobile={this.mobile}
          onAction={($event: CustomEvent<UIEvent>) => this.justifyContent($event.detail, ContentAlign.CENTER)}
          class={this.contentAlign === ContentAlign.CENTER ? 'active' : undefined}>
          <deckgo-ie-action-image cssClass={'center-align'}></deckgo-ie-action-image>
        </deckgo-ie-action-button>
        <deckgo-ie-action-button
          mobile={this.mobile}
          onAction={($event: CustomEvent<UIEvent>) => this.justifyContent($event.detail, ContentAlign.RIGHT)}
          class={this.contentAlign === ContentAlign.RIGHT ? 'active' : undefined}>
          <deckgo-ie-action-image cssClass={'right-align'}></deckgo-ie-action-image>
        </deckgo-ie-action-button>
      </Host>
    );
  }
}
