import {StorageFile, TenorGif, UnsplashPhoto, Waves} from '@deckdeckgo/editor';
import {EditAction} from './edit-action';

export interface ImageAction {
  action: EditAction;
  image?: UnsplashPhoto | TenorGif | StorageFile | Waves;
}
