import {Component, Element, EventEmitter, h, Prop} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

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
      return <h2>{i18n.state.editor.qr_code_options}</h2>;
    } else if (this.selectedElement.slide?.chart) {
      return <h2>{i18n.state.editor.chart_options}</h2>;
    } else if (this.selectedElement.slide?.author) {
      return <h2>{i18n.state.editor.author_options}</h2>;
    } else {
      return <h2>{i18n.state.editor.slide_options}</h2>;
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
      return <app-edit-slide-user selectedElement={this.selectedElement} slideDidChange={this.slideDidChange}></app-edit-slide-user>;
    }
  }
}
