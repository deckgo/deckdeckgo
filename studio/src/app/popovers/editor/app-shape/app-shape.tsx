import {Component, Element, h, Prop, State} from '@stencil/core';

import {TargetElement} from '../../../utils/editor/target-element';
import {ImageAction} from '../../../utils/editor/image-action';

import {EnvironmentDeckDeckGoConfig} from '../../../services/core/environment/environment-config';
import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {AssetsService} from '../../../services/core/assets/assets.service';

@Component({
  tag: 'app-shape',
  styleUrl: 'app-shape.scss'
})
export class AppShape {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private applyToTargetElement: TargetElement = TargetElement.SHAPES;

  @State()
  private assets: Assets | undefined = undefined;

  private config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  async componentWillLoad() {
    this.assets = await AssetsService.getInstance().assets();
  }

  private async closePopoverWithoutResults() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async selectApplyToTargetElement($event: CustomEvent<TargetElement>) {
    if ($event && $event.detail) {
      this.applyToTargetElement = $event.detail;
    }
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
        ratio
      }
    });
  }

  private async selectImage($event: CustomEvent<ImageAction>) {
    if ($event && $event.detail) {
      await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
        img: $event.detail
      });
    }
  }

  render() {
    return [
      <ion-toolbar>
        <h2>Add a shape</h2>
        <ion-router-link slot="end" onClick={() => this.closePopoverWithoutResults()}>
          <ion-icon name="close"></ion-icon>
        </ion-router-link>
      </ion-toolbar>,
      <app-select-target-element
        shapes={true}
        images={true}
        onApplyTo={($event: CustomEvent<TargetElement>) => this.selectApplyToTargetElement($event)}></app-select-target-element>,
      this.renderShapes(),
      this.renderImages()
    ];
  }

  private renderImages() {
    if (this.applyToTargetElement !== TargetElement.IMAGES) {
      return undefined;
    }

    return (
      <app-image
        selectedElement={this.selectedElement}
        slide={true}
        deleteBackground={false}
        onAction={($event: CustomEvent<ImageAction>) => this.selectImage($event)}></app-image>
    );
  }

  private renderShapes() {
    if (this.applyToTargetElement !== TargetElement.SHAPES) {
      return undefined;
    }

    return (
      <div class="container ion-margin-bottom">
        <ion-list>
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
    if (this.assets && this.assets.shapes && this.assets.shapes[group] && this.assets.shapes[group].length > 0) {
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
