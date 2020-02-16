import {Component, Element, EventEmitter, h, Prop} from '@stencil/core';

@Component({
  tag: 'app-shape',
  styleUrl: 'app-shape.scss'
})
export class AppShape {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  codeDidChange: EventEmitter<HTMLElement>;

  // TODO replace with config.globalAssetsUrl
  // private config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  private async closePopoverWithoutResults() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    return [
      <ion-toolbar>
        <h2>Add a shape</h2>
        <ion-router-link slot="end" onClick={() => this.closePopoverWithoutResults()}>
          <ion-icon name="close"></ion-icon>
        </ion-router-link>
      </ion-toolbar>,
      <div class="container ion-margin-bottom">
        <ion-list>
          <ion-item-divider>
            <ion-label>Shapes</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderImage(`/assets/img/shapes/circle-solid.svg`, 'Circle solid')}
            {this.renderImage(`/assets/img/shapes/circle-regular.svg`, 'Circle solid')}
            {this.renderImage(`/assets/img/shapes/square-solid.svg`, 'Square solid')}
            {this.renderImage(`/assets/img/shapes/square-regular.svg`, 'Square regular')}
            {this.renderImage(`/assets/img/shapes/heart-solid.svg`, 'Heart solid')}
            {this.renderImage(`/assets/img/shapes/heart-regular.svg`, 'Heart regular')}
            {this.renderImage(`/assets/img/shapes/star-solid.svg`, 'Star solid')}
            {this.renderImage(`/assets/img/shapes/star-regular.svg`, 'Star regular')}
            {this.renderImage(`/assets/img/shapes/bookmark-solid.svg`, 'Bookmark solid')}
            {this.renderImage(`/assets/img/shapes/bookmark-regular.svg`, 'Bookmark regular')}
            {this.renderImage(`/assets/img/shapes/certificate-solid.svg`, 'Certificate')}
            {this.renderImage(`/assets/img/shapes/cloud-solid.svg`, 'Cloud')}
            {this.renderImage(`/assets/img/shapes/cog-solid.svg`, 'Cog')}
            {this.renderImage(`/assets/img/shapes/shopping-cart-solid.svg`, 'Shopping cart')}
            {this.renderImage(`/assets/img/shapes/tag-solid.svg`, 'Tag')}
            {this.renderImage(`/assets/img/shapes/trophy-solid.svg`, 'Trophy')}
            {this.renderImage(`/assets/img/shapes/bell-solid.svg`, 'Bell solid')}
            {this.renderImage(`/assets/img/shapes/bell-regular.svg`, 'Bell regular')}
            {this.renderImage(`/assets/img/shapes/building-solid.svg`, 'Building solid')}
            {this.renderImage(`/assets/img/shapes/building-regular.svg`, 'Building regular')}
            {this.renderImage(`/assets/img/shapes/square-full-solid.svg`, 'Square full')}
            {this.renderImage(`/assets/img/shapes/map-marker-solid.svg`, 'Map marker')}
            {this.renderImage(`/assets/img/shapes/play-solid.svg`, 'Play')}
            {this.renderImage(`/assets/img/shapes/camera-solid.svg`, 'Camera')}
          </div>

          <ion-item-divider>
            <ion-label>Arrows</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderImage(`/assets/img/shapes/angle-left-solid.svg`, 'Angle leeft')}
            {this.renderImage(`/assets/img/shapes/angle-right-solid.svg`, 'Angle right')}
            {this.renderImage(`/assets/img/shapes/angle-double-left-solid.svg`, 'Angle left double')}
            {this.renderImage(`/assets/img/shapes/angle-double-right-solid.svg`, 'Angle right double')}
            {this.renderImage(`/assets/img/shapes/arrow-left-solid.svg`, 'Arrow left')}
            {this.renderImage(`/assets/img/shapes/arrow-right-solid.svg`, 'Arrow right')}
            {this.renderImage(`/assets/img/shapes/arrow-alt-circle-left-solid.svg`, 'Arrow circe left solid')}
            {this.renderImage(`/assets/img/shapes/arrow-alt-circle-right-solid.svg`, 'Arrow circe right solid')}
            {this.renderImage(`/assets/img/shapes/arrow-alt-circle-left-regular.svg`, 'Arrow circle left regular')}
            {this.renderImage(`/assets/img/shapes/arrow-alt-circle-right-regular.svg`, 'Arrow circle right regular')}
            {this.renderImage(`/assets/img/shapes/chevron-left-solid.svg`, 'Chevron left')}
            {this.renderImage(`/assets/img/shapes/chevron-right-solid.svg`, 'Chevron right')}
            {this.renderImage(`/assets/img/shapes/long-arrow-alt-left-solid.svg`, 'Long arrow left')}
            {this.renderImage(`/assets/img/shapes/long-arrow-alt-right-solid.svg`, 'Long arrow right')}
            {this.renderImage(`/assets/img/shapes/arrows-alt-h-solid.svg`, 'Arrow h')}
            {this.renderImage(`/assets/img/shapes/arrows-alt-v-solid.svg`, 'Arrow v')}
            {this.renderImage(`/assets/img/shapes/expand-alt-solid.svg`, 'Expand')}
            {this.renderImage(`/assets/img/shapes/compress-alt-solid.svg`, 'Compress')}
            {this.renderImage(`/assets/img/shapes/expand-arrows-alt-solid.svg`, 'Expand arrows')}
            {this.renderImage(`/assets/img/shapes/compress-arrows-alt-solid.svg`, 'Compress arrows')}
            {this.renderImage(`/assets/img/shapes/exchange-alt-solid.svg`, 'Exchange')}
            {this.renderImage(`/assets/img/shapes/random-solid.svg`, 'Random')}
            {this.renderImage(`/assets/img/shapes/redo-solid.svg`, 'Redo')}
            {this.renderImage(`/assets/img/shapes/undo-solid.svg`, 'Undo')}
            {this.renderImage(`/assets/img/shapes/sync-solid.svg`, 'Sync')}
          </div>

          <ion-item-divider>
            <ion-label>Status</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderImage(`/assets/img/shapes/check-circle-solid.svg`, 'Check circle solid')}
            {this.renderImage(`/assets/img/shapes/check-circle-regular.svg`, 'Check circle regular')}
            {this.renderImage(`/assets/img/shapes/check-solid.svg`, 'Check')}
            {this.renderImage(`/assets/img/shapes/check-square-solid.svg`, 'Check square')}
            {this.renderImage(`/assets/img/shapes/info-solid.svg`, 'Info')}
            {this.renderImage(`/assets/img/shapes/info-circle-solid.svg`, 'Info circle')}
            {this.renderImage(`/assets/img/shapes/minus-circle-solid.svg`, 'Minus circle')}
            {this.renderImage(`/assets/img/shapes/plus-circle-solid.svg`, 'Plus circle')}
            {this.renderImage(`/assets/img/shapes/unlock-solid.svg`, 'Unlock')}
            {this.renderImage(`/assets/img/shapes/lock-solid.svg`, 'Lock')}
            {this.renderImage(`/assets/img/shapes/ban-solid.svg`, 'Ban')}
            {this.renderImage(`/assets/img/shapes/plus-solid.svg`, 'Plus')}
            {this.renderImage(`/assets/img/shapes/plus-square-solid.svg`, 'Plus square solid')}
            {this.renderImage(`/assets/img/shapes/plus-square-regular.svg`, 'Plus square regular')}
          </div>

          <ion-item-divider>
            <ion-label>Computers</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderImage(`/assets/img/shapes/desktop-solid.svg`, 'Desktop')}
            {this.renderImage(`/assets/img/shapes/laptop-solid.svg`, 'Laptop')}
            {this.renderImage(`/assets/img/shapes/tablet-solid.svg`, 'Tablet')}
            {this.renderImage(`/assets/img/shapes/mobile-solid.svg`, 'Mobile')}
            {this.renderImage(`/assets/img/shapes/server-solid.svg`, 'Server')}
            {this.renderImage(`/assets/img/shapes/database-solid.svg`, 'Database')}
            {this.renderImage(`/assets/img/shapes/download-solid.svg`, 'Download')}
            {this.renderImage(`/assets/img/shapes/envelope-solid.svg`, 'Envelope')}
            {this.renderImage(`/assets/img/shapes/power-off-solid.svg`, 'Power off')}
            {this.renderImage(`/assets/img/shapes/rocket-solid.svg`, 'Rocket')}
            {this.renderImage(`/assets/img/shapes/robot-solid.svg`, 'Robot')}
          </div>

          <ion-item-divider>
            <ion-label>Date and Time</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderImage(`/assets/img/shapes/calendar-solid.svg`, 'Calendar solid')}
            {this.renderImage(`/assets/img/shapes/calendar-regular.svg`, 'Calendar regular')}
            {this.renderImage(`/assets/img/shapes/clock-solid.svg`, 'Clock solid')}
            {this.renderImage(`/assets/img/shapes/clock-regular.svg`, 'Clock regular')}
          </div>

          <ion-item-divider>
            <ion-label>Files</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderImage(`/assets/img/shapes/copy-solid.svg`, 'Copy solid')}
            {this.renderImage(`/assets/img/shapes/copy-regular.svg`, 'Copy regular')}
            {this.renderImage(`/assets/img/shapes/clipboard-solid.svg`, 'Clipboard solid')}
            {this.renderImage(`/assets/img/shapes/clipboard-regular.svg`, 'Clipboard regular')}
            {this.renderImage(`/assets/img/shapes/file-solid.svg`, 'File solid')}
            {this.renderImage(`/assets/img/shapes/file-regular.svg`, 'File regular')}
            {this.renderImage(`/assets/img/shapes/folder-solid.svg`, 'Folder solid')}
            {this.renderImage(`/assets/img/shapes/folder-regular.svg`, 'Folder regular')}
            {this.renderImage(`/assets/img/shapes/cut-solid.svg`, 'Cut')}
          </div>

          <ion-item-divider>
            <ion-label>Finance</ion-label>
          </ion-item-divider>

          <div class="ion-padding shapes">
            {this.renderImage(`/assets/img/shapes/chart-line-solid.svg`, 'Chart line')}
            {this.renderImage(`/assets/img/shapes/chart-pie-solid.svg`, 'Chart pie')}
            {this.renderImage(`/assets/img/shapes/credit-card-solid.svg`, 'Credit card solid')}
            {this.renderImage(`/assets/img/shapes/credit-card-regular.svg`, 'Credit card regular')}
            {this.renderImage(`/assets/img/shapes/cc-amazon-pay-brands.svg`, 'Amazon Pay')}
            {this.renderImage(`/assets/img/shapes/cc-apple-pay-brands.svg`, 'Apple Pay')}
            {this.renderImage(`/assets/img/shapes/cc-paypal-brands.svg`, 'Paypal')}
            {this.renderImage(`/assets/img/shapes/cc-stripe-brands.svg`, 'Stripe')}
            {this.renderImage(`/assets/img/shapes/bitcoin-brands.svg`, 'Bitcoin')}
          </div>
        </ion-list>
      </div>
    ];
  }

  private renderImage(src: string, ariaLabel: string) {
    return (
      <div>
        <div class="image-container">
          <deckgo-lazy-img slot="icon" svg-src={src} aria-label={ariaLabel}></deckgo-lazy-img>
        </div>
      </div>
    );
  }
}
