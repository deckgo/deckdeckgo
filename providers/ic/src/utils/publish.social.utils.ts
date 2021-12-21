import {PublishData} from '@deckdeckgo/editor';

import {StorageUpload} from './publish.utils';
import {upload} from './storage.utils';

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
    filename: `${social_image_name}.png`,
    folder: 'meta',
    storageActor: actor,
    headers: [['Cache-Control', 'max-age=3600']]
  });
};
