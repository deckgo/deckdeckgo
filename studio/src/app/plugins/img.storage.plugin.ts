import {StorageFile} from '@deckdeckgo/editor';
import {uploadOfflineFile} from '@deckdeckgo/offline';
import {StyloPlugin, StyloPluginCreateParagraphsParams} from '@papyrs/stylo';
import {createParagraphImage} from '../utils/editor/plugin.utils';

export const imgStorage: StyloPlugin = {
  text: 'image',
  icon: 'img',
  files: {
    accept: 'image/x-png,image/jpeg,image/gif,image/svg+xml,image/webp',
    multiple: false
  },
  createParagraphs: async ({container, paragraph, files}: StyloPluginCreateParagraphsParams) => {
    const storageFile: StorageFile = await uploadOfflineFile(files[0], 'images', 10485760);

    createParagraphImage({
      image: storageFile,
      container,
      paragraph
    });
  }
};
