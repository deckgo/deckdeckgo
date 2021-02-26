import {Component, Element, Fragment, h, Prop} from '@stencil/core';

import {InitTemplate} from '../../../../utils/editor/create-slides.utils';

import {AppTemplatesFixed} from '../../../../components/editor/templates/platform/app-templates-fixed/app-templates-fixed';

@Component({
  tag: 'app-transform-slide',
  styleUrl: 'app-transform-slide.scss',
})
export class AppTransformSlide {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  private async closePopover(template?: InitTemplate) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      template,
    });
  }

  private selectTemplate = async (template: InitTemplate) => {
    await this.closePopover(template);
  };

  render() {
    return (
      <Fragment>
        <ion-toolbar>
          <h2>Transform slide</h2>
          <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
        </ion-toolbar>

        <div class="container">
          <AppTemplatesFixed selectTemplate={this.selectTemplate}></AppTemplatesFixed>
        </div>
      </Fragment>
    );
  }
}
