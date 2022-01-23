import {StyloPlugin, StyloPluginCreateParagraphsParams} from '@deckdeckgo/stylo';

import i18n from '../stores/i18n.store';

import {openCodeModal} from '../utils/editor/plugin.utils';

export const code: StyloPlugin = {
  text: i18n.state.editor.code,
  icon: 'code',
  createParagraphs: (pluginParams: StyloPluginCreateParagraphsParams) => openCodeModal({pluginParams})
};
