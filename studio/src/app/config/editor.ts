import {h1, h2, h3, StyloConfig, ul} from '@papyrs/stylo';

import {imgStorage} from '../plugins/img.storage.plugin';
import {imgUnsplash} from '../plugins/img.unsplash.plugin';
import {imgGif} from '../plugins/img.gif.plugin';
import {code} from '../plugins/code.plugin';
import {hr} from '../plugins/hr.plugin';

import {codeMenu} from '../menus/code.menu';

export const editorConfig: Partial<StyloConfig> = {
  plugins: [h1, h2, h3, ul, imgStorage, imgUnsplash, imgGif, code, hr],
  menus: [codeMenu]
};
