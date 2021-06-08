import {Component, Element, Fragment, h, Prop} from '@stencil/core';

import i18n from '../../../stores/i18n.store';
import {renderI18n} from '../../../utils/core/i18n.utils';

import settingsStore from '../../../stores/settings.store';

@Component({
  tag: 'app-slide-warning-info',
  styleUrl: 'app-slide-warning-info.scss',
})
export class AppSlideWarningInfo {
  @Element() el: HTMLElement;

  @Prop()
  lowContrast: boolean;

  @Prop()
  overflow: boolean;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async deactivateContrastWarning() {
    settingsStore.state.contrastWarning = false;
    await this.closePopover();
  }

  render() {
    return (
      <div class="ion-padding">
        {this.renderLowContrast()}

        {this.renderOverflow()}

        <div class="ion-text-center">
          <ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>
            {i18n.state.core.got_it}
          </ion-button>
        </div>

        {this.renderDeactivateLowContrast()}
      </div>
    );
  }

  private renderOverflow() {
    if (!this.overflow) {
      return undefined;
    }

    return (
      <Fragment>
        <h2>{i18n.state.warning.overflow}</h2>
        <p>{i18n.state.warning.overflow_explanation}</p>
        <p>{i18n.state.warning.overflow_visibility}</p>
      </Fragment>
    );
  }

  private renderLowContrast() {
    if (!this.lowContrast) {
      return undefined;
    }

    return (
      <Fragment>
        <h2>{i18n.state.warning.low_contrast}</h2>
        <p>{i18n.state.warning.contrast_ratio}</p>
        <p>
          {renderI18n(i18n.state.warning.wcag, {
            placeholder: '{0}',
            value: (
              <a href="https://www.w3.org/TR/WCAG/#contrast-minimum" target="_blank" rel="noopener noreferrer">
                WCAG
              </a>
            ),
          })}
        </p>

        <p>{i18n.state.warning.note}</p>
      </Fragment>
    );
  }

  private renderDeactivateLowContrast() {
    if (!this.lowContrast) {
      return undefined;
    }

    return <div class="ion-text-center">
      <button
        onClick={async () => await this.deactivateContrastWarning()}>
        {i18n.state.settings.deactivate_contrast_warning}
      </button>
    </div>
  }
}
