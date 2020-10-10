import {EditAction} from './edit-action';

export interface ImageAction {
  action: EditAction;
  image?: UnsplashPhoto | TenorGif | StorageFile | SvgWaves;
}
