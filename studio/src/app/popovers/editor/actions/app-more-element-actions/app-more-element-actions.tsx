import {Component, Element, h, Prop} from '@stencil/core';
import i18n from '../../../../stores/i18n.store';
import {MoreAction} from '../../../../types/editor/more-action';

@Component({
  tag: 'app-more-element-actions',
  styleUrl: 'app-more-element-actions.scss'
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
      action: action
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
      <a onClick={() => this.closePopover(MoreAction.NOTES)} aria-label={i18n.state.editor.notes}>
        <p>{i18n.state.editor.notes}</p>
      </a>
    );
  }

  private renderClone() {
    if (!this.clone) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.CLONE)} aria-label={i18n.state.editor.copy}>
        <p>{i18n.state.editor.copy}</p>
      </a>
    );
  }

  private renderDelete() {
    return (
      <a onClick={() => this.closePopover(MoreAction.DELETE)} aria-label={i18n.state.core.delete}>
        <p>{i18n.state.core.delete}</p>
      </a>
    );
  }

  private renderImages() {
    if (!this.images) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.IMAGES)} aria-label={i18n.state.editor.add_image}>
        <p>{i18n.state.editor.add_image}</p>
      </a>
    );
  }

  private renderTransform() {
    if (!this.transform) {
      return undefined;
    }

    return (
      <a onClick={() => this.closePopover(MoreAction.TRANSFORM)} aria-label={i18n.state.editor.transform}>
        <p>{i18n.state.editor.transform}</p>
      </a>
    );
  }
}
