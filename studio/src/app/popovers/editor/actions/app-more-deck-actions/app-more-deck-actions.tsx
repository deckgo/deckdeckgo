import {Component, Element, h, Prop} from '@stencil/core';

import {MoreAction} from '../../../../utils/editor/more-action';

@Component({
  tag: 'app-more-deck-actions',
  styleUrl: 'app-more-deck-actions.scss',
})
export class AppMoreDeckActions {
  @Element() el: HTMLElement;

  @Prop()
  offline: boolean = false;

  private async closePopover(action: MoreAction) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      action: action,
    });
  }

  private async closeSharePopover($event: CustomEvent<MoreAction>) {
    await this.closePopover($event ? $event.detail : null);
  }

  render() {
    return (
      <div class="ion-padding">
        <a onClick={() => this.closePopover(MoreAction.JUMP_TO)} aria-label="Jump to slide">
          <p>Slides</p>
        </a>

        <a onClick={() => this.closePopover(MoreAction.PRESENT)}>
          <p>Present</p>
        </a>

        <app-share-options onSelectedOption={($event: CustomEvent<MoreAction>) => this.closeSharePopover($event)}></app-share-options>

        <a onClick={() => this.closePopover(MoreAction.OFFLINE)} aria-label="Offline">
          <p>{this.offline ? 'Go Online' : 'Go Offline'}</p>
        </a>

        <app-action-help link={true} onHelpSelected={() => this.closePopover(MoreAction.HELP)}></app-action-help>
      </div>
    );
  }
}
