import {Component, Element, EventEmitter, h, Prop} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {EditAction} from '../../../../types/editor/edit-action';

import {SelectedTarget} from '../../../../types/editor/selected-target';

@Component({
  tag: 'app-edit-slide',
  styleUrl: 'app-edit-slide.scss'
})
export class AppEditSlide {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: SelectedTarget;

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
      action: $event.detail
    };

    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss(data);
  }

  render() {
    return [
      <ion-toolbar>
        {this.renderTitle()}
        <app-close-menu slot="end" onClose={() => this.closePopoverWithoutResults()}></app-close-menu>
      </ion-toolbar>,
      <ion-list class="article">{this.renderOptions()}</ion-list>
    ];
  }

  private renderTitle() {
    if (this.selectedTarget.slide?.qrCode) {
      return <h2>{i18n.state.editor.qr_code_options}</h2>;
    } else if (this.selectedTarget.slide?.chart) {
      return <h2>{i18n.state.editor.chart_options}</h2>;
    } else if (this.selectedTarget.slide?.author) {
      return <h2>{i18n.state.editor.author_options}</h2>;
    } else {
      return <h2>{i18n.state.editor.slide_options}</h2>;
    }
  }

  private renderOptions() {
    if (this.selectedTarget.slide?.qrCode) {
      return (
        <app-edit-slide-qrcode
          selectedTarget={this.selectedTarget.target}
          slideDidChange={this.slideDidChange}
          onAction={($event: CustomEvent<EditAction>) => this.closePopover($event)}
        ></app-edit-slide-qrcode>
      );
    } else if (this.selectedTarget.slide?.chart) {
      return (
        <app-edit-slide-chart
          selectedTarget={this.selectedTarget.target}
          slideDidChange={this.slideDidChange}
          onAction={($event: CustomEvent<EditAction>) => this.closePopover($event)}
        ></app-edit-slide-chart>
      );
    } else if (this.selectedTarget.slide?.author) {
      return (
        <app-edit-slide-author selectedTarget={this.selectedTarget.target} slideDidChange={this.slideDidChange}></app-edit-slide-author>
      );
    } else {
      return <app-edit-slide-user selectedTarget={this.selectedTarget} slideDidChange={this.slideDidChange}></app-edit-slide-user>;
    }
  }
}
