import {Component, Element, h, Prop, State} from '@stencil/core';

import {EnvironmentDeckDeckGoConfig} from '../../../services/core/environment/environment-config';
import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {AssetsService} from '../../../services/core/assets/assets.service';

@Component({
  tag: 'app-shape',
  styleUrl: 'app-shape.scss',
})
export class AppShape {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private assets: Assets | undefined = undefined;

  private config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  async componentWillLoad() {
    this.assets = await AssetsService.getInstance().assets();
  }

  private async closePopoverWithoutResults() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async selectShape(src: string, label: string) {
    const lazyImgElement: HTMLElement = this.el.querySelector(`deckgo-lazy-img[svg-src='${src}']`);
    const ratio: number =
      lazyImgElement && lazyImgElement.parentElement
        ? lazyImgElement.parentElement.getBoundingClientRect().height / lazyImgElement.parentElement.getBoundingClientRect().width
        : 1;

    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      svg: {
        src,
        label,
        ratio,
      },
    });
  }

  render() {
    return [
      <ion-toolbar>
        <h2>Add a shape</h2>
        <button slot="end" class="close-options" onClick={() => this.closePopoverWithoutResults()} tabindex={0}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </button>
      </ion-toolbar>,
      this.renderShapes(),
    ];
  }

  private renderShapes() {
    return (
      <div class="container ion-margin-bottom">
        <ion-list class="article">
          <ion-item-divider>
            <ion-label>Shapes</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">{this.renderShapesGroup('shapes')}</div>

          <ion-item-divider>
            <ion-label>Arrows</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">{this.renderShapesGroup('arrows')}</div>

          <ion-item-divider>
            <ion-label>Status</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">{this.renderShapesGroup('status')}</div>

          <ion-item-divider>
            <ion-label>Computers</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">{this.renderShapesGroup('computers')}</div>

          <ion-item-divider>
            <ion-label>Date and Time</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">{this.renderShapesGroup('dateTime')}</div>

          <ion-item-divider>
            <ion-label>Files</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">{this.renderShapesGroup('files')}</div>

          <ion-item-divider>
            <ion-label>Finance</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">{this.renderShapesGroup('finance')}</div>
        </ion-list>
      </div>
    );
  }

  private renderShapesGroup(group: string) {
    if (this.assets?.shapes?.[group]?.length > 0) {
      return this.assets.shapes[group].map((asset: ImgAsset) => {
        return this.renderShape(`${this.config.globalAssetsUrl}${asset.src}`, asset.ariaLabel);
      });
    } else {
      return undefined;
    }
  }

  private renderShape(src: string, ariaLabel: string) {
    return (
      <div custom-tappable onClick={() => this.selectShape(src, ariaLabel)}>
        <div class="image-container">
          <deckgo-lazy-img svg-src={src} aria-label={ariaLabel}></deckgo-lazy-img>
        </div>
      </div>
    );
  }
}
