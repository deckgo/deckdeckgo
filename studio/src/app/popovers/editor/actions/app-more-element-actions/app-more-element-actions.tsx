import {Component, Element, h, Prop} from '@stencil/core';
import {MoreAction} from '../../../../types/editor/more-action';

@Component({
  tag: 'app-more-element-actions',
  styleUrl: 'app-more-element-actions.scss',
})
export class AppMoreElementActions {
  @Element() el: HTMLElement;

  @Prop()
  notes: boolean = false;

  @Prop()
  clone: boolean = false;

  @Prop()
  images: boolean = false;

  @Prop()
  transform: boolean = false;

  private async closePopover(action: MoreAction) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      action: action,
    });
  }

  render() {
    return (
      <div class="ion-padding">
        {this.renderImages()}
        {this.renderNotes()}
        {this.renderClone()}
        {this.renderTransform()}
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

  private renderClone() {
    if (!this.clone) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.CLONE)} aria-label="Copy">
        <p>Copy</p>
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

  private renderImages() {
    if (!this.images) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.IMAGES)} aria-label="Add an image">
        <p>Add image</p>
      </a>
    );
  }

  private renderTransform() {
    if (!this.transform) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.TRANSFORM)} aria-label="Transform">
        <p>Transform</p>
      </a>
    );
  }
}
