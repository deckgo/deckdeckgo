import {StyloPlugin, StyloPluginCreateParagraphsParams} from '@papyrs/stylo';
import {openCodeModal} from '../utils/editor/plugin.utils';

export const code: StyloPlugin = {
  text: 'code',
  icon: 'code',
  createParagraphs: (pluginParams: StyloPluginCreateParagraphsParams) => openCodeModal({pluginParams})
};
