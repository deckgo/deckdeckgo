import {Component, Element, Fragment, h, Prop, Event, EventEmitter} from '@stencil/core';

import i18n from '../../../stores/i18n.store';
import {renderI18n} from '../../../utils/core/i18n.utils';

@Component({
  tag: 'app-slide-warning-info',
})
export class AppSlideWarningInfo {
  @Element() el: HTMLElement;

  @Prop()
  lowContrast: boolean;

  @Prop()
  overflow: boolean;

  @Event() deactivateContrastWarning: EventEmitter<boolean>;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async setCheckContrastWarning() {
    this.deactivateContrastWarning.emit(false);
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
        <div class="ion-text-center">
          <ion-button size="small" shape="round" color="warning" onClick={() => this.setCheckContrastWarning()}>
            {i18n.state.settings.deactivate_contrast_warning}
          </ion-button>
        </div>
      </Fragment>
    );
  }
}
