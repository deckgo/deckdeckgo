import {Component, ComponentInterface, h, Host, Prop} from '@stencil/core';
import {AppIcon} from '../../app-icon/app-icon';
import {StorageFile} from '@deckdeckgo/editor';

@Component({
  tag: 'app-asset-data',
  styleUrl: 'app-asset-data.scss'
})
export class AppAssetData implements ComponentInterface {
  @Prop()
  data!: StorageFile;

  render() {
    return (
      <Host>
        <AppIcon name="document" ariaHidden={true} ariaLabel=""></AppIcon>
        <ion-label>{this.data.name}</ion-label>
      </Host>
    );
  }
}
