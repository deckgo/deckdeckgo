import {Component, Element, EventEmitter, h, Prop} from '@stencil/core';

import {EditAction} from '../../../../utils/editor/edit-action';

@Component({
  tag: 'app-edit-slide',
  styleUrl: 'app-edit-slide.scss',
})
export class AppEditSlide {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  qrCode: boolean;

  @Prop()
  chart: boolean;

  @Prop()
  author: boolean;

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
        <ion-router-link slot="end" onClick={() => this.closePopoverWithoutResults()}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </ion-router-link>
      </ion-toolbar>,
      <ion-list class="article">{this.renderOptions()}</ion-list>,
    ];
  }

  private renderTitle() {
    if (this.qrCode) {
      return <h2>QR code options</h2>;
    } else if (this.chart) {
      return <h2>Chart options</h2>;
    } else if (this.author) {
      return <h2>Author options</h2>;
    } else {
      return <h2>Slide options</h2>;
    }
  }

  private renderOptions() {
    if (this.qrCode) {
      return (
        <app-edit-slide-qrcode
          selectedElement={this.selectedElement}
          slideDidChange={this.slideDidChange}
          onAction={($event: CustomEvent<EditAction>) => this.closePopover($event)}></app-edit-slide-qrcode>
      );
    } else if (this.chart) {
      return (
        <app-edit-slide-chart
          selectedElement={this.selectedElement}
          slideDidChange={this.slideDidChange}
          onAction={($event: CustomEvent<EditAction>) => this.closePopover($event)}></app-edit-slide-chart>
      );
    } else if (this.author) {
      return <app-edit-slide-author selectedElement={this.selectedElement} slideDidChange={this.slideDidChange}></app-edit-slide-author>;
    } else {
      return undefined;
    }
  }
}
