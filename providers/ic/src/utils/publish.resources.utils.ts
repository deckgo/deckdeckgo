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

const getAuthor = (): string => EnvStore.getInstance().get().author;

export const uploadResources = async ({meta}: {meta: Meta | undefined}) => {
  // 1. Get actor
  const {actor}: BucketActor<StorageBucketActor> = await getStorageActor();

  // 2. Get already uploaded assets
  const assetKeys: AssetKey[] = await actor.list(toNullable<string>('resources'));
  const keys: string[] = assetKeys.map(({name}: AssetKey) => name);

  // 3. Get list of resources - i.e. the kit
  const kit: Kit[] = await getKit();

  // 4. We only upload resources that have not been yet uploaded. In other words: we upload the resources the first time or if hashes are modified.
  const kitNewFiles: Kit[] = kit.filter(({filename}: Kit) => !keys.includes(filename));

  if (!kitNewFiles || kitNewFiles.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = kitNewFiles.map((kit: Kit) => addKitIC({kit, actor, meta}));
  await Promise.all(promises);

  // If there was an update, we ensure we also update the sw list
  await addSwKitIC({kitNewFiles, kit, actor, meta});
};

const addKitIC = async ({kit, actor, meta}: {kit: Kit; actor: StorageBucketActor; meta: Meta | undefined}) => {
  const {src, filename, mimeType, updateContent, headers} = kit;

  const content: string = await downloadKit(src);

  const updatedContent: string = updateContent ? updateContent({content, meta}) : content;

  await uploadKit({filename, content: updatedContent, actor, mimeType, headers, fullPath: src.replace(getKitPath(), '')});
};

const addSwKitIC = async ({
  kitNewFiles,
  kit,
  actor,
  meta
}: {
  kitNewFiles: Kit[];
  kit: Kit[];
  actor: StorageBucketActor;
  meta: Meta | undefined;
}) => {
  const sw: Kit | undefined = kitNewFiles.find(({filename}: Kit) => filename === 'service-worker.js');

  if (sw !== undefined) {
    return;
  }

  const swKit: Kit | undefined = kit.find(({filename}: Kit) => filename === 'service-worker.js');

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

const getKit = async (): Promise<Kit[]> => {
  const kitPath: string = getKitPath();

  const resources: string[] = await (await fetch(`${kitPath}/build.json`)).json();

  const toResource = (resource: string): Partial<Kit> => {
    const src: string = `${kitPath}/${resource}`;

    if (src.includes('.js')) {
      return {
        src,
        mimeType: 'text/javascript'
      };
    }

    if (src.includes('.css')) {
      return {
        src,
        mimeType: 'text/css'
      };
    }

    if (src.includes('.webmanifest')) {
      return {
        src,
        mimeType: 'application/manifest+json',
        updateContent: ({content, meta}: {meta: Meta | undefined; content: string}) =>
          content.replace('{{DECKDECKGO_AUTHOR}}', meta?.author?.name || getAuthor())
      };
    }

    return {
      src,
      mimeType: 'text/plain'
    };
  };

  return resources
    .map((src: string) => toResource(src))
    .map((resource: Partial<Kit>) => {
      const {pathname}: URL = new URL(resource.src);
      return {
        ...resource,
        filename: pathname.split('/').pop(),
        headers: [['Cache-Control', 'max-age=31536000']]
      } as Kit;
    });
};
