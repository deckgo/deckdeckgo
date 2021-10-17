import {Component, ComponentInterface, h, Prop} from '@stencil/core';

import {StorageFile} from '@deckdeckgo/editor';

import {AppIcon} from '../../app-icon/app-icon';

@Component({
  tag: 'app-asset-data',
  styleUrl: 'app-asset-data.scss'
})
export class AppAssetData implements ComponentInterface {
  @Prop()
  data!: StorageFile;

  render() {
    return <AppIcon name="document" ariaHidden={true} ariaLabel=""></AppIcon>;
  }
}
