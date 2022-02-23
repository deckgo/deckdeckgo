import {StyloPlugin, StyloPluginCreateParagraphsParams} from '@papyrs/stylo';
import {openPluginModal} from '../utils/editor/plugin.utils';

export const imgUnsplash: StyloPlugin = {
  text: 'stock_photo',
  icon: `<svg width="32" height="32" viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg">
      <path d="M10 9V0h12v9H10zm12 5h10v18H0V14h10v9h12v-9z" fill="currentColor" fill-rule="nonzero" />
    </svg>
  `,
  createParagraphs: (pluginParams: StyloPluginCreateParagraphsParams) => openPluginModal({pluginParams, componentTag: 'app-unsplash'})
};
