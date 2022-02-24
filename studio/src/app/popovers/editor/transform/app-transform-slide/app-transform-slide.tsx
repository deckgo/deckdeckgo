import {Component, Element, Fragment, h, Prop} from '@stencil/core';
import {AppTemplatesFixed} from '../../../../components/editor/deck/slide/templates/platform/app-templates-fixed/app-templates-fixed';
import i18n from '../../../../stores/i18n.store';
import {InitTemplate} from '../../../../utils/editor/create-slides.utils';

@Component({
  tag: 'app-transform-slide',
  styleUrl: 'app-transform-slide.scss'
})
export class AppTransformSlide {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  private async closePopover(template?: InitTemplate) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      template
    });
  }

  private selectTemplate = async (template: InitTemplate) => {
    await this.closePopover(template);
  };

  render() {
    const tag: 'title' | 'content-default' | 'content-bottom' | 'split-horizontal' | 'split-vertical' =
      this.selectedTarget.tagName.toLowerCase() === 'deckgo-slide-title'
        ? 'title'
        : this.selectedTarget.tagName.toLowerCase() === 'deckgo-slide-content'
        ? this.selectedTarget.style?.getPropertyValue('--slide-content-justify-content') !== ''
          ? 'content-bottom'
          : 'content-default'
        : this.selectedTarget.tagName.toLowerCase() === 'deckgo-slide-split' && this.selectedTarget.hasAttribute('vertical')
        ? 'split-vertical'
        : 'split-horizontal';

    return (
      <Fragment>
        <ion-toolbar>
          <h2>{i18n.state.editor.transform_slide}</h2>
          <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
        </ion-toolbar>

        <div class="container">
          <AppTemplatesFixed selectTemplate={this.selectTemplate} selected={tag}></AppTemplatesFixed>
        </div>
      </Fragment>
    );
  }
}
