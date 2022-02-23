import {Component, Element, h} from '@stencil/core';
import {MoreAction} from '../../../../types/editor/more-action';

@Component({
  tag: 'app-more-share-options'
})
export class AppMoreShareOptions {
  @Element() el: HTMLElement;

  private async closePopover($event: CustomEvent<MoreAction>) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      action: $event ? $event.detail : null
    });
  }

  render() {
    return (
      <div class="ion-padding">
        <app-share-options onSelectedOption={($event: CustomEvent<MoreAction>) => this.closePopover($event)}></app-share-options>
      </div>
    );
  }
}
