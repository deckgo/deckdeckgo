import {h1, h2, h3, StyloConfig, ul} from '@deckdeckgo/stylo';

import {imgStorage} from '../plugins/img.storage.plugin';
import {imgUnsplash} from '../plugins/img.unsplash.plugin';
import {imgGif} from '../plugins/img.gif.plugin';
import {code} from '../plugins/code.plugin';
import {hr} from '../plugins/hr.plugin';

export const editorConfig: Partial<StyloConfig> = {
  plugins: [h1, h2, h3, ul, imgStorage, imgUnsplash, imgGif, code, hr],
  menus: [
    {
      match: ({paragraph}: {paragraph: HTMLElement}) => paragraph?.nodeName.toLowerCase() === 'deckgo-highlight-code',
      actions: [
        {
          text: 'edit_code',
          icon: `<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20' viewBox='0 0 512 512'>
            <polygon points='364.13 125.25 87 403 64 448 108.99 425 386.75 147.87 364.13 125.25' style='fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px'/>
            <path d='M420.69,68.69,398.07,91.31l22.62,22.63,22.62-22.63a16,16,0,0,0,0-22.62h0A16,16,0,0,0,420.69,68.69Z' style='fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px'/>
          </svg>`,
          action: async ({paragraph}: {paragraph: HTMLElement}) => {
            const editCode: CustomEvent<void> = new CustomEvent<void>('editCode', {
              bubbles: true
            });

            paragraph.dispatchEvent(editCode);
          }
        }
      ]
    }
  ]
};
