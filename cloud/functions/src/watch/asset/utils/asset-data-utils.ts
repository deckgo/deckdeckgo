import {extractAssetPath} from './asset-utils';

export  function getDataPath(attributes: any): Promise<string | undefined> {
    return new Promise<string|undefined>(async (resolve) => {
        if (attributes === undefined || attributes['src'] === undefined || attributes['src'] === '') {
            resolve(undefined);
            return;
        }

        const path: string | null = attributes['src'];
        const assetPath: string | undefined = await extractAssetPath(path);

        resolve(assetPath);
    });
}
