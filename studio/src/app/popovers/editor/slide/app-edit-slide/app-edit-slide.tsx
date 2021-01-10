import {Component, Element, EventEmitter, h, Prop} from '@stencil/core';

import {EditAction} from '../../../../types/editor/edit-action';

import {SelectedElement} from '../../../../types/editor/selected-element';

@Component({
  tag: 'app-edit-slide',
  styleUrl: 'app-edit-slide.scss',
})
export class AppEditSlide {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: SelectedElement;

  @Prop()
  slideDidChange: EventEmitter<HTMLElement>;

  private async closePopoverWithoutResults() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async closePopover($event: CustomEvent<EditAction>) {
    if (!$event) {
      return;
    }

    const data = {
      action: $event.detail,
    };

    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss(data);
  }

  render() {
    return [
      <ion-toolbar>
        {this.renderTitle()}
        <app-close-menu slot="end" onClose={() => this.closePopoverWithoutResults()}></app-close-menu>
      </ion-toolbar>,
      <ion-list class="article">{this.renderOptions()}</ion-list>,
    ];
  }

  private renderTitle() {
    if (this.selectedElement.slide?.qrCode) {
      return <h2>QR code options</h2>;
    } else if (this.selectedElement.slide?.chart) {
      return <h2>Chart options</h2>;
    } else if (this.selectedElement.slide?.author) {
      return <h2>Author options</h2>;
    } else {
      return <h2>Slide options</h2>;
    }
  }

  private renderOptions() {
    if (this.selectedElement.slide?.qrCode) {
      return (
        <app-edit-slide-qrcode
          selectedElement={this.selectedElement.element}
          slideDidChange={this.slideDidChange}
          onAction={($event: CustomEvent<EditAction>) => this.closePopover($event)}></app-edit-slide-qrcode>
      );
    } else if (this.selectedElement.slide?.chart) {
      return (
        <app-edit-slide-chart
          selectedElement={this.selectedElement.element}
          slideDidChange={this.slideDidChange}
          onAction={($event: CustomEvent<EditAction>) => this.closePopover($event)}></app-edit-slide-chart>
      );
    } else if (this.selectedElement.slide?.author) {
      return <app-edit-slide-author selectedElement={this.selectedElement.element} slideDidChange={this.slideDidChange}></app-edit-slide-author>;
    } else {
      return undefined;
    }
  }
}
