import {h1, h2, h3, StyloConfig, ul} from '@papyrs/stylo';
import {codeMenu} from '../menus/code.menu';
import {code} from '../plugins/code.plugin';
import {hr} from '../plugins/hr.plugin';
import {imgGif} from '../plugins/img.gif.plugin';
import {imgStorage} from '../plugins/img.storage.plugin';
import {imgUnsplash} from '../plugins/img.unsplash.plugin';

export const editorConfig: Partial<StyloConfig> = {
  plugins: [h1, h2, h3, ul, imgStorage, imgUnsplash, imgGif, code, hr],
  menus: [codeMenu],
  excludeAttributes: ['id', 'hydrated', 'editable', 'paragraph_id', 'highlighted', 'custom-loader']
};
