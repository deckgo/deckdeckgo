import {Component, Element, Fragment, h, Prop} from '@stencil/core';
import {SlotType} from '../../../../types/editor/slot-type';

@Component({
  tag: 'app-transform-slide',
  styleUrl: 'app-transform-slide.scss',
})
export class AppTransformSlide {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  private async closePopover(type?: SlotType) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      type: type,
    });
  }

  render() {
    return (
      <Fragment>
        <ion-toolbar>
          <h2>Transform slide</h2>
          <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
        </ion-toolbar>

        <app-templates-fixed></app-templates-fixed>
      </Fragment>
    );
  }
}
