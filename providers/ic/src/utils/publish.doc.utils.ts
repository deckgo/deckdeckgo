import {Doc, DocData, docPublishData, DocPublishData, PublishData} from '@deckdeckgo/editor';

import {initIndexHTML, initUpload, StorageUpload, updateMetaData, uploadPublishFileIC} from './publish.utils';
import {setData} from './data.utils';
import {uploadSocialImage} from './publish.social.utils';

export const publishDoc = async ({
  doc: docSource
}: {
  doc: Doc;
}): Promise<{doc: Doc; storageUpload: StorageUpload; publishData: PublishData}> => {
  const {id, data} = docSource;
  const {meta} = data;

  // 1. Init and fill HTML
  const indexHTML: {html: string; publishData: DocPublishData} = await initDocIndexHTML({doc: docSource});
  const {storageUpload, publishData} = await initUpload({indexHTML, folder: 'd', meta});

  // 2. Update doc published meta
  const docData: DocData = updateMetaData<DocData>({data, meta: data.meta, name: data.name, storageUpload});

  // 3. Update doc meta information
  const doc: Doc = await setData<Doc, DocData>({key: `/docs/${id}`, id, data: docData});

  // 4. Upload
  await uploadPublishFileIC(storageUpload);

  // 5. Upload
  await uploadSocialImage({storageUpload, publishData});

  return {
    storageUpload,
    publishData,
    doc
  };
};

const initDocIndexHTML = async ({doc}: {doc: Doc}): Promise<{html: string; publishData: DocPublishData}> => {
  const publishData: DocPublishData = await docPublishData({doc});

  const {paragraphs} = publishData;

  const updateTemplateContent = ({attr, template}: {attr: string | undefined; template: string}): string =>
    template.replace('<!-- DECKDECKGO_DOC -->', `<deckgo-doc ${attr || ''}><article>${paragraphs.join('')}</article></deckgo-doc>`);

  const {html}: {html: string} = await initIndexHTML({publishData, updateTemplateContent, sourceFolder: 'd'});

  return {
    html,
    publishData
  };
};

export const emitDocPublished = (doc: Doc) => {
  const {id, data} = doc;

  const deployedDoc: Doc = {
    id,
    data: {
      ...data,
      deploy: {
        api: {
          status: 'successful',
          updated_at: new Date()
        }
      }
    }
  };

  const $event: CustomEvent<Doc> = new CustomEvent('docPublished', {detail: deployedDoc});
  document.dispatchEvent($event);
};
