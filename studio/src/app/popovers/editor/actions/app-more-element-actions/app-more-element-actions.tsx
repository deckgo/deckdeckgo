import {Component, Element, h, Prop} from '@stencil/core';
import {MoreAction} from '../../../../utils/editor/more-action';

@Component({
  tag: 'app-more-element-actions',
  styleUrl: 'app-more-element-actions.scss',
})
export class AppMoreElementActions {
  @Element() el: HTMLElement;

  @Prop()
  notes: boolean = false;

  @Prop()
  copy: boolean = false;

  @Prop()
  reveal: boolean = false;

  @Prop()
  list: boolean = false;

  private async closePopover(action: MoreAction) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      action: action,
    });
  }

  render() {
    return (
      <div class="ion-padding">
        {this.renderNotes()}
        {this.renderCopy()}
        {this.renderReveal()}
        {this.renderList()}
        {this.renderDelete()}
      </div>
    );
  }

  private renderNotes() {
    if (!this.notes) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.NOTES)} aria-label="Notes">
        <p>Notes</p>
      </a>
    );
  }

  private renderCopy() {
    if (!this.copy) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.COPY)} aria-label="Copy">
        <p>Copy</p>
      </a>
    );
  }

  private renderList() {
    if (!this.list) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.LIST)} aria-label="List">
        <p>List</p>
      </a>
    );
  }

  private renderReveal() {
    if (!this.reveal) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.REVEAL)} aria-label="Animation">
        <p>Animation</p>
      </a>
    );
  }

  private renderDelete() {
    return (
      <a onClick={() => this.closePopover(MoreAction.DELETE)} aria-label="Delete">
        <p>Delete</p>
      </a>
    );
  }
}
