import {BehaviorSubject, Observable} from 'rxjs';

import {take} from 'rxjs/operators';

import {Asset, AssetData} from '../../../models/data/asset';

import {AssetService} from '../../data/asset/asset.service';
import {ErrorService} from '../../core/error/error.service';

export class AssetEditorService {

    private assetSubject: BehaviorSubject<Asset> = new BehaviorSubject(null);

    private static instance: AssetEditorService;

    private assetService: AssetService;
    private errorService: ErrorService;

    private constructor() {
        this.assetService = AssetService.getInstance();
        this.errorService = ErrorService.getInstance();
    }

    static getInstance() {
        if (!AssetEditorService.instance) {
            AssetEditorService.instance = new AssetEditorService();
        }
        return AssetEditorService.instance;
    }

    private watch(): Observable<Asset> {
        return this.assetSubject.asObservable();
    }

    private next(asset: Asset) {
        this.assetSubject.next(asset);
    }

    init(deckId: string | undefined): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                if (!deckId) {
                    this.assetSubject.next(null);
                } else {
                    const asset: Asset = await this.assetService.get(deckId);
                    this.assetSubject.next(asset);
                }

                resolve();
            } catch (err) {
                this.errorService.error(err);
                resolve();
            }
        });
    }

    addImage(deckId: string, ownerId: string, addImagePath: string, removeImagePath?: string): Promise<void> {
        return this.add(deckId, ownerId, 'images', addImagePath, removeImagePath);
    }

    addData(deckId: string, ownerId: string, addImagePath: string, removeImagePath?: string): Promise<void> {
        return this.add(deckId, ownerId, 'data', addImagePath, removeImagePath);
    }

    removeImage(deckId: string, removeImagePath: string): Promise<void> {
        return this.remove(deckId, 'images', removeImagePath);
    }

    removeData(deckId: string, removeImagePath: string): Promise<void> {
        return this.remove(deckId, 'data', removeImagePath);
    }

    private add(deckId: string, ownerId: string, container: string, addPath: string, removePath?: string): Promise<void> {
        return new Promise<void>((resolve, reject) => {
            try {
                if (!addPath || addPath === undefined || addPath === '') {
                    resolve();
                    return;
                }

                this.watch().pipe(take(1)).subscribe(async (asset: Asset) => {
                    const assetData: AssetData = (asset && asset !== undefined) ? asset.data : {owner_id: ownerId};

                    if (!assetData[container] || assetData[container].length <= 0) {
                        assetData[container] = [];
                    }

                    if (removePath && removePath !== undefined && removePath.length > 0) {
                        if (assetData[container] && assetData[container].indexOf(removePath) > -1) {
                            assetData[container].splice(assetData[container].indexOf(removePath), 1);
                        }
                    }

                    assetData[container].push(addPath);

                    let updatedAsset: Asset;
                    if (asset && asset.id) {
                        asset.data = assetData;
                        updatedAsset = await this.assetService.update(deckId, asset);
                    } else {
                        updatedAsset = await this.assetService.create(deckId, assetData);
                    }

                    this.next(updatedAsset);

                    resolve();
                });
            } catch (err) {
                reject(err);
            }
        });
    }

    private remove(deckId: string, container: string, removePath: string): Promise<void> {
        return new Promise<void>((resolve, reject) => {
            try {
                if (!removePath || removePath === undefined || removePath === '') {
                    resolve();
                    return;
                }

                this.watch().pipe(take(1)).subscribe(async (asset: Asset) => {
                    if (!asset) {
                        asset = await this.assetService.get(deckId);
                    }

                    if (!asset || !asset.data || !asset.data[container] || asset.data[container].length <= 0) {
                        resolve();
                        return;
                    }

                    if (asset.data[container] && asset.data[container].indexOf(removePath) > -1) {
                        asset.data[container].splice(asset.data[container].indexOf(removePath), 1);

                        // TODO: Don't keep empty array?

                        const updatedAsset: Asset = await this.assetService.update(deckId, asset);
                        this.next(updatedAsset);
                    }

                    resolve();
                });
            } catch (err) {
                reject(err);
            }
        });
    }
}
