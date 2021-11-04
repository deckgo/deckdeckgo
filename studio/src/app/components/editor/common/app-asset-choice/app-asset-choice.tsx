import {Fragment, FunctionalComponent, h} from '@stencil/core';

import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';

import offlineStore from '../../../../stores/offline.store';
import i18n from '../../../../stores/i18n.store';

import {EditAction} from '../../../../types/editor/edit-action';

import {tenor, unsplash} from '../../../../utils/core/environment.utils';

const tenorEnabled = tenor();
const unsplashEnabled = unsplash();

interface AppAssetChoiceProps {
  selectAction: (action: EditAction, image?: UnsplashPhoto | TenorGif | StorageFile | Waves) => Promise<void>;
}

export const AppAssetChoice: FunctionalComponent<AppAssetChoiceProps> = ({selectAction}) => {
  const renderStockPhotos = () => {
    if (!offlineStore.state.online) {
      // Unsplash not available offline
      return undefined;
    }

    if (!unsplashEnabled) {
      return undefined;
    }

    return (
      <ion-button shape="round" onClick={async () => await selectAction(EditAction.OPEN_UNSPLASH)} color="primary">
        <ion-label>{i18n.state.editor.stock_photo}</ion-label>
      </ion-button>
    );
  };

  const renderGif = () => {
    if (!offlineStore.state.online) {
      // Tenor not available offline
      return undefined;
    }

    if (!tenorEnabled) {
      return undefined;
    }

    return (
      <ion-button shape="round" onClick={async () => await selectAction(EditAction.OPEN_GIFS)} color="secondary">
        <ion-label>{i18n.state.editor.gif}</ion-label>
      </ion-button>
    );
  };

  const renderCustom = () => {
    return (
      <ion-button shape="round" onClick={async () => await selectAction(EditAction.OPEN_CUSTOM)} color="tertiary">
        <ion-label>{i18n.state.editor.your_images}</ion-label>
      </ion-button>
    );
  };

  return (
    <Fragment>
      {renderStockPhotos()}
      {renderGif()}
      {renderCustom()}
    </Fragment>
  );
};
