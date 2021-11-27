import {Principal} from '@dfinity/principal';

import {_SERVICE as StorageBucketActor, AssetKey} from '../canisters/storage/storage.did';

import {getPublishBucket} from './publish.utils';
import {toNullable} from './did.utils';
import {upload} from './storage.utils';

type KitMimeType = 'text/javascript' | 'text/plain' | 'application/manifest+json' | 'text/css';

interface Kit {
  src: string;
  filename: string;
  mimeType: KitMimeType;
}

const kitPath: string = 'https://raw.githubusercontent.com/deckgo/ic-kit/main/dist/';

const kit: Kit[] = [
  {
    src: `${kitPath}workbox-c837f436.js`,
    mimeType: 'text/javascript'
  },
  {
    src: `${kitPath}service-worker.js`,
    mimeType: 'text/javascript'
  },
  {
    src: `${kitPath}robots.txt`,
    mimeType: 'text/plain'
  },
  {
    src: `${kitPath}manifest.webmanifest`,
    mimeType: 'application/manifest+json'
  },
  {
    src: `${kitPath}build/index.css`,
    mimeType: 'text/css'
  },
  {
    src: `${kitPath}build/index-KBD4IHFC.js`,
    mimeType: 'text/javascript'
  },
  {
    src: `${kitPath}build/deck/index.css`,
    mimeType: 'text/css'
  },
  {
    src: `${kitPath}build/deck/index-VTCYBTMV.js`,
    mimeType: 'text/javascript'
  }
].map((resource: {src: string; mimeType: KitMimeType}) => {
  const {pathname}: URL = new URL(resource.src);
  return {
    ...resource,
    filename: pathname.split('/').pop()
  } as Kit;
});

export const uploadResources = async () => {
  // 1. Get actor
  const {actor}: {bucket: Principal; actor: StorageBucketActor} = await getPublishBucket();

  const assetKeys: AssetKey[] = await actor.list(toNullable<string>('resources'));
  const keys: string[] = assetKeys.map(({name}: AssetKey) => name);

  // We only upload resources that have not been yet uploaded. In other words: we upload the resources the first time or if hashes are modified.
  const kitFiles: Kit[] = kit.filter(({filename}: Kit) => !keys.includes(filename));

  if (!kitFiles || kitFiles.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = kitFiles.map((kit: Kit) => addKitIC({kit, actor}));
  await Promise.all(promises);
};

const addKitIC = async ({kit, actor}: {kit: Kit; actor: StorageBucketActor}) => {
  const {src, filename, mimeType} = kit;

  const content: string = await downloadKit(src);
  await uploadKit({filename, content, actor, mimeType, fullPath: src.replace(kitPath, '')});
};

const uploadKit = async ({
  filename,
  fullPath,
  content,
  actor,
  mimeType
}: {
  filename: string;
  fullPath: string;
  content: string;
  actor: StorageBucketActor;
  mimeType: KitMimeType;
}): Promise<void> => {
  await upload({
    data: new Blob([content], {type: mimeType}),
    filename,
    folder: 'resources',
    storageBucket: actor,
    fullPath
  });
};

const downloadKit = async (src: string): Promise<string> => {
  const htmlTemplate: Response = await fetch(src);
  return htmlTemplate.text();
};
