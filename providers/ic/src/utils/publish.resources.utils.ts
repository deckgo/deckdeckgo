import {log, Meta} from '@deckdeckgo/editor';
import {AssetKey, HeaderField, _SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';
import {EnvStore} from '../stores/env.store';
import {toNullable} from './did.utils';
import {BucketActor} from './manager.utils';
import {getStorageActor, upload} from './storage.utils';

type KitMimeType = 'text/javascript' | 'text/plain' | 'application/manifest+json' | 'text/css';

interface Kit {
  src: string;
  filename: string;
  mimeType: KitMimeType;
  headers: HeaderField[];
  updateContent?: ({content, meta}: {meta: Meta | undefined; content: string}) => string;
}

const getKitPath = (): string => EnvStore.getInstance().get().kitPath;

const kit = (): Kit[] => {
  const kitPath: string = getKitPath();

  return [
    {
      src: `${kitPath}/workbox-c837f436.js`,
      mimeType: 'text/javascript'
    },
    {
      src: `${kitPath}/service-worker.js`,
      mimeType: 'text/javascript'
    },
    {
      src: `${kitPath}/robots.txt`,
      mimeType: 'text/plain'
    },
    {
      src: `${kitPath}/manifest.webmanifest`,
      mimeType: 'application/manifest+json',
      updateContent: ({content, meta}: {meta: Meta | undefined; content: string}) =>
        content.replace('{{DECKDECKGO_AUTHOR}}', meta?.author?.name || 'DeckDeckGo')
    },
    {
      src: `${kitPath}/build/index-M763JKRZ.css`,
      mimeType: 'text/css'
    },
    {
      src: `${kitPath}/build/index-55VE2P7H.js`,
      mimeType: 'text/javascript'
    },
    {
      src: `${kitPath}/build/deck/index-ELSPH57U.js`,
      mimeType: 'text/javascript'
    },
    {
      src: `${kitPath}/build/deck/index-HPLMJ2FO.css`,
      mimeType: 'text/css'
    },
    {
      src: `${kitPath}/build/doc/index-RZB27UR5.css`,
      mimeType: 'text/css'
    }
  ].map((resource: {src: string; mimeType: KitMimeType}) => {
    const {pathname}: URL = new URL(resource.src);
    return {
      ...resource,
      filename: pathname.split('/').pop(),
      headers: [['Cache-Control', 'max-age=31536000']]
    } as Kit;
  });
};

export const uploadResources = async ({meta}: {meta: Meta | undefined}) => {
  // 1. Get actor
  const {actor}: BucketActor<StorageBucketActor> = await getStorageActor();

  const assetKeys: AssetKey[] = await actor.list(toNullable<string>('resources'));
  const keys: string[] = assetKeys.map(({name}: AssetKey) => name);

  // We only upload resources that have not been yet uploaded. In other words: we upload the resources the first time or if hashes are modified.
  const kitFiles: Kit[] = kit().filter(({filename}: Kit) => !keys.includes(filename));

  if (!kitFiles || kitFiles.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = kitFiles.map((kit: Kit) => addKitIC({kit, actor, meta}));
  await Promise.all(promises);

  // If there was an update, we ensure we also update the sw list
  await addSwKitIC({kitFiles, actor, meta});
};

const addKitIC = async ({kit, actor, meta}: {kit: Kit; actor: StorageBucketActor; meta: Meta | undefined}) => {
  const {src, filename, mimeType, updateContent, headers} = kit;

  const content: string = await downloadKit(src);

  const updatedContent: string = updateContent ? updateContent({content, meta}) : content;

  await uploadKit({filename, content: updatedContent, actor, mimeType, headers, fullPath: src.replace(getKitPath(), '')});
};

const addSwKitIC = async ({kitFiles, actor, meta}: {kitFiles: Kit[]; actor: StorageBucketActor; meta: Meta | undefined}) => {
  const sw: Kit | undefined = kitFiles.find(({filename}: Kit) => filename === 'service-worker.js');

  if (sw !== undefined) {
    return;
  }

  const swKit: Kit | undefined = kit().find(({filename}: Kit) => filename === 'service-worker.js');

  if (!swKit !== undefined) {
    return;
  }

  await addKitIC({kit: swKit, actor, meta});
};

const uploadKit = async ({
  filename,
  fullPath,
  content,
  actor,
  mimeType,
  headers
}: {
  filename: string;
  fullPath: string;
  content: string;
  actor: StorageBucketActor;
  mimeType: KitMimeType;
  headers: HeaderField[];
}): Promise<void> => {
  await upload({
    data: new Blob([content], {type: mimeType}),
    filename,
    folder: 'resources',
    storageActor: actor,
    fullPath,
    headers,
    log
  });
};

const downloadKit = async (src: string): Promise<string> => {
  const htmlTemplate: Response = await fetch(src);
  return htmlTemplate.text();
};
