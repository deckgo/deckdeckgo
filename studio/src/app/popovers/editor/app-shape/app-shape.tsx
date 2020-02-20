import {Component, Element, h, Prop, State} from '@stencil/core';
import {TargetElement} from '../../../utils/editor/target-element';
import {ImageAction} from '../../../utils/editor/image-action';

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

  // TODO replace with config.globalAssetsUrl
  // private config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

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

          <div class="ion-padding shapes">
            {this.renderShape(`/assets/img/shapes/circle-solid.svg`, 'Circle solid')}
            {this.renderShape(`/assets/img/shapes/circle-regular.svg`, 'Circle solid')}
            {this.renderShape(`/assets/img/shapes/square-solid.svg`, 'Square solid')}
            {this.renderShape(`/assets/img/shapes/square-regular.svg`, 'Square regular')}
            {this.renderShape(`/assets/img/shapes/heart-solid.svg`, 'Heart solid')}
            {this.renderShape(`/assets/img/shapes/heart-regular.svg`, 'Heart regular')}
            {this.renderShape(`/assets/img/shapes/star-solid.svg`, 'Star solid')}
            {this.renderShape(`/assets/img/shapes/star-regular.svg`, 'Star regular')}
            {this.renderShape(`/assets/img/shapes/bookmark-solid.svg`, 'Bookmark solid')}
            {this.renderShape(`/assets/img/shapes/bookmark-regular.svg`, 'Bookmark regular')}
            {this.renderShape(`/assets/img/shapes/certificate-solid.svg`, 'Certificate')}
            {this.renderShape(`/assets/img/shapes/cloud-solid.svg`, 'Cloud')}
            {this.renderShape(`/assets/img/shapes/user-solid.svg`, 'User solid')}
            {this.renderShape(`/assets/img/shapes/user-regular.svg`, 'User regular')}
            {this.renderShape(`/assets/img/shapes/building-solid.svg`, 'Building solid')}
            {this.renderShape(`/assets/img/shapes/building-regular.svg`, 'Building regular')}
            {this.renderShape(`/assets/img/shapes/bell-solid.svg`, 'Bell solid')}
            {this.renderShape(`/assets/img/shapes/bell-regular.svg`, 'Bell regular')}
            {this.renderShape(`/assets/img/shapes/cog-solid.svg`, 'Cog')}
            {this.renderShape(`/assets/img/shapes/shopping-cart-solid.svg`, 'Shopping cart')}
            {this.renderShape(`/assets/img/shapes/tag-solid.svg`, 'Tag')}
            {this.renderShape(`/assets/img/shapes/trophy-solid.svg`, 'Trophy')}
            {this.renderShape(`/assets/img/shapes/square-full-solid.svg`, 'Square full')}
            {this.renderShape(`/assets/img/shapes/map-marker-solid.svg`, 'Map marker')}
            {this.renderShape(`/assets/img/shapes/play-solid.svg`, 'Play')}
            {this.renderShape(`/assets/img/shapes/camera-solid.svg`, 'Camera')}
          </div>

          <ion-item-divider>
            <ion-label>Arrows</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderShape(`/assets/img/shapes/angle-left-solid.svg`, 'Angle leeft')}
            {this.renderShape(`/assets/img/shapes/angle-right-solid.svg`, 'Angle right')}
            {this.renderShape(`/assets/img/shapes/angle-double-left-solid.svg`, 'Angle left double')}
            {this.renderShape(`/assets/img/shapes/angle-double-right-solid.svg`, 'Angle right double')}
            {this.renderShape(`/assets/img/shapes/arrow-left-solid.svg`, 'Arrow left')}
            {this.renderShape(`/assets/img/shapes/arrow-right-solid.svg`, 'Arrow right')}
            {this.renderShape(`/assets/img/shapes/arrow-alt-circle-left-solid.svg`, 'Arrow circe left solid')}
            {this.renderShape(`/assets/img/shapes/arrow-alt-circle-right-solid.svg`, 'Arrow circe right solid')}
            {this.renderShape(`/assets/img/shapes/arrow-alt-circle-left-regular.svg`, 'Arrow circle left regular')}
            {this.renderShape(`/assets/img/shapes/arrow-alt-circle-right-regular.svg`, 'Arrow circle right regular')}
            {this.renderShape(`/assets/img/shapes/chevron-left-solid.svg`, 'Chevron left')}
            {this.renderShape(`/assets/img/shapes/chevron-right-solid.svg`, 'Chevron right')}
            {this.renderShape(`/assets/img/shapes/long-arrow-alt-left-solid.svg`, 'Long arrow left')}
            {this.renderShape(`/assets/img/shapes/long-arrow-alt-right-solid.svg`, 'Long arrow right')}
            {this.renderShape(`/assets/img/shapes/arrows-alt-h-solid.svg`, 'Arrow h')}
            {this.renderShape(`/assets/img/shapes/arrows-alt-v-solid.svg`, 'Arrow v')}
            {this.renderShape(`/assets/img/shapes/expand-alt-solid.svg`, 'Expand')}
            {this.renderShape(`/assets/img/shapes/compress-alt-solid.svg`, 'Compress')}
            {this.renderShape(`/assets/img/shapes/expand-arrows-alt-solid.svg`, 'Expand arrows')}
            {this.renderShape(`/assets/img/shapes/compress-arrows-alt-solid.svg`, 'Compress arrows')}
            {this.renderShape(`/assets/img/shapes/exchange-alt-solid.svg`, 'Exchange')}
            {this.renderShape(`/assets/img/shapes/random-solid.svg`, 'Random')}
            {this.renderShape(`/assets/img/shapes/redo-solid.svg`, 'Redo')}
            {this.renderShape(`/assets/img/shapes/undo-solid.svg`, 'Undo')}
            {this.renderShape(`/assets/img/shapes/sync-solid.svg`, 'Sync')}
          </div>

          <ion-item-divider>
            <ion-label>Status</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderShape(`/assets/img/shapes/check-circle-solid.svg`, 'Check circle solid')}
            {this.renderShape(`/assets/img/shapes/check-circle-regular.svg`, 'Check circle regular')}
            {this.renderShape(`/assets/img/shapes/check-square-solid.svg`, 'Check square solid')}
            {this.renderShape(`/assets/img/shapes/check-square-regular.svg`, 'Check square regular')}
            {this.renderShape(`/assets/img/shapes/times-circle-solid.svg`, 'Times square solid')}
            {this.renderShape(`/assets/img/shapes/times-circle-regular.svg`, 'Times square regular')}
            {this.renderShape(`/assets/img/shapes/plus-circle-solid.svg`, 'Plus circle')}
            {this.renderShape(`/assets/img/shapes/minus-circle-solid.svg`, 'Minus circle')}
            {this.renderShape(`/assets/img/shapes/plus-square-solid.svg`, 'Plus square solid')}
            {this.renderShape(`/assets/img/shapes/plus-square-regular.svg`, 'Plus square regular')}
            {this.renderShape(`/assets/img/shapes/minus-square-solid.svg`, 'Minus square solid')}
            {this.renderShape(`/assets/img/shapes/minus-square-regular.svg`, 'Minus square regular')}
            {this.renderShape(`/assets/img/shapes/unlock-solid.svg`, 'Unlock')}
            {this.renderShape(`/assets/img/shapes/lock-solid.svg`, 'Lock')}
            {this.renderShape(`/assets/img/shapes/info-solid.svg`, 'Info')}
            {this.renderShape(`/assets/img/shapes/info-circle-solid.svg`, 'Info circle')}
            {this.renderShape(`/assets/img/shapes/check-solid.svg`, 'Check')}
            {this.renderShape(`/assets/img/shapes/times-solid.svg`, 'Times')}
            {this.renderShape(`/assets/img/shapes/plus-solid.svg`, 'Plus')}
            {this.renderShape(`/assets/img/shapes/minus-solid.svg`, 'Minus')}
            {this.renderShape(`/assets/img/shapes/ban-solid.svg`, 'Ban')}
          </div>

          <ion-item-divider>
            <ion-label>Computers</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderShape(`/assets/img/shapes/desktop-solid.svg`, 'Desktop')}
            {this.renderShape(`/assets/img/shapes/laptop-solid.svg`, 'Laptop')}
            {this.renderShape(`/assets/img/shapes/tablet-solid.svg`, 'Tablet')}
            {this.renderShape(`/assets/img/shapes/mobile-solid.svg`, 'Mobile')}
            {this.renderShape(`/assets/img/shapes/server-solid.svg`, 'Server')}
            {this.renderShape(`/assets/img/shapes/database-solid.svg`, 'Database')}
            {this.renderShape(`/assets/img/shapes/download-solid.svg`, 'Download')}
            {this.renderShape(`/assets/img/shapes/envelope-solid.svg`, 'Envelope')}
            {this.renderShape(`/assets/img/shapes/power-off-solid.svg`, 'Power off')}
            {this.renderShape(`/assets/img/shapes/rocket-solid.svg`, 'Rocket')}
            {this.renderShape(`/assets/img/shapes/robot-solid.svg`, 'Robot')}
          </div>

          <ion-item-divider>
            <ion-label>Date and Time</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderShape(`/assets/img/shapes/calendar-solid.svg`, 'Calendar solid')}
            {this.renderShape(`/assets/img/shapes/calendar-regular.svg`, 'Calendar regular')}
            {this.renderShape(`/assets/img/shapes/clock-solid.svg`, 'Clock solid')}
            {this.renderShape(`/assets/img/shapes/clock-regular.svg`, 'Clock regular')}
          </div>

          <ion-item-divider>
            <ion-label>Files</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderShape(`/assets/img/shapes/copy-solid.svg`, 'Copy solid')}
            {this.renderShape(`/assets/img/shapes/copy-regular.svg`, 'Copy regular')}
            {this.renderShape(`/assets/img/shapes/clipboard-solid.svg`, 'Clipboard solid')}
            {this.renderShape(`/assets/img/shapes/clipboard-regular.svg`, 'Clipboard regular')}
            {this.renderShape(`/assets/img/shapes/file-solid.svg`, 'File solid')}
            {this.renderShape(`/assets/img/shapes/file-regular.svg`, 'File regular')}
            {this.renderShape(`/assets/img/shapes/folder-solid.svg`, 'Folder solid')}
            {this.renderShape(`/assets/img/shapes/folder-regular.svg`, 'Folder regular')}
            {this.renderShape(`/assets/img/shapes/cut-solid.svg`, 'Cut')}
          </div>

          <ion-item-divider>
            <ion-label>Finance</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderShape(`/assets/img/shapes/chart-line-solid.svg`, 'Chart line')}
            {this.renderShape(`/assets/img/shapes/chart-pie-solid.svg`, 'Chart pie')}
            {this.renderShape(`/assets/img/shapes/credit-card-solid.svg`, 'Credit card solid')}
            {this.renderShape(`/assets/img/shapes/credit-card-regular.svg`, 'Credit card regular')}
            {this.renderShape(`/assets/img/shapes/cc-amazon-pay-brands.svg`, 'Amazon Pay')}
            {this.renderShape(`/assets/img/shapes/cc-apple-pay-brands.svg`, 'Apple Pay')}
            {this.renderShape(`/assets/img/shapes/cc-paypal-brands.svg`, 'Paypal')}
            {this.renderShape(`/assets/img/shapes/cc-stripe-brands.svg`, 'Stripe')}
            {this.renderShape(`/assets/img/shapes/bitcoin-brands.svg`, 'Bitcoin')}
          </div>
        </ion-list>
      </div>
    );
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
