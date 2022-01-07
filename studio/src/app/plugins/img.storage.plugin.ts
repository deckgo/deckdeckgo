import {StyloPlugin, StyloPluginCreateParagraphsParams, createEmptyElement, transformParagraph} from '@deckdeckgo/stylo';
import i18n from '../stores/i18n.store';
import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';
import {SlotType} from '../types/editor/slot-type';
import {initDeckgoLazyImgAttributes} from '../utils/editor/image.utils';
import {StorageOfflineProvider} from '../providers/storage/storage.offline.provider';

export const insertImage = ({
  image,
  paragraph,
  container
}: {
  image: UnsplashPhoto | TenorGif | StorageFile;
  paragraph: HTMLElement;
  container: HTMLElement;
}) => {
  const deckgoImg: HTMLDeckgoLazyImgElement = document.createElement(SlotType.IMG);

  const img: HTMLDeckgoLazyImgElement = initDeckgoLazyImgAttributes({
    element: deckgoImg,
    image
  });

  const emptyDiv: HTMLElement = createEmptyElement({nodeName: 'div'});

  transformParagraph({
    elements: [img, emptyDiv],
    paragraph,
    container,
    focus: 'last'
  });
};

export const imgStorage: StyloPlugin = {
  text: i18n.state.editor.image,
  icon: 'img',
  files: {
    accept: 'image/x-png,image/jpeg,image/gif,image/svg+xml,image/webp',
    multiple: false
  },
  createParagraphs: async ({container, paragraph, files}: StyloPluginCreateParagraphsParams) => {
    const storageFile: StorageFile = await StorageOfflineProvider.getInstance().uploadFile(files[0], 'images', 10485760);

    insertImage({
      image: storageFile,
      container,
      paragraph
    });
  }
};
