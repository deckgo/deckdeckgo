import {PublishData} from '@deckdeckgo/editor';

import {StorageUpload} from './publish.utils';
import {upload} from './storage.utils';

const socialImageFolder: string = 'meta';
const socialImageExtension: string = 'png';

export const updateTemplateSocialImage = ({html, data, bucketUrl}: {html: string; data: PublishData; bucketUrl: string}): string => {
  const {social_image_name} = data;

  const pathname: string = `/${socialImageFolder}/${social_image_name}.${socialImageExtension}`;

  return html.replaceAll('{{DECKDECKGO_SOCIAL_IMAGE}}', `${bucketUrl}${pathname}`);
};

export const uploadSocialImage = async ({
  storageUpload,
  publishData
}: {
  storageUpload: StorageUpload;
  publishData: PublishData;
}): Promise<void> => {
  const {social_image_name, social_image_value} = publishData;

  if (!social_image_value) {
    return;
  }

  const {actor} = storageUpload;

  await upload({
    data: social_image_value,
    filename: `${social_image_name}.${socialImageExtension}`,
    folder: socialImageFolder,
    storageActor: actor,
    headers: [['Cache-Control', 'max-age=3600']]
  });
};
