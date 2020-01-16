import {take} from 'rxjs/operators';

import {Deck} from '../../../../models/data/deck';

import {Resources} from '../../../../utils/core/resources';

import {AssetImageAction} from '../../../../utils/editor/asset-action';

import {DeckEditorService} from '../../../../services/editor/deck/deck-editor.service';
import {AssetEditorService} from '../../../../services/editor/asset/asset-editor.service';
import {EnvironmentConfigService} from '../../../../services/core/environment/environment-config.service';

export class AssetEventsHandler {

    private deckEditorService: DeckEditorService;
    private assetEditorService: AssetEditorService;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
        this.assetEditorService = AssetEditorService.getInstance();
    }

    // TODO: Delete image
    // TODO: Append data
    // TODO: Delete data

    init(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (document) {
                document.addEventListener('assetImageChange', this.onAppendImage, false);
            }

            resolve();
        });
    }

    destroy() {
        if (document) {
            document.removeEventListener('assetImageChange', this.onAppendImage, false);
        }
    }

    private onAppendImage = async ($event: CustomEvent) => {
        if (!$event || !$event.detail) {
            return;
        }

        await this.appendImage($event.detail);
    };

    private appendImage(assetAction: AssetImageAction): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                if (deck && deck.id && deck.data && deck.data.owner_id) {
                    const oldPath: string | undefined = await this.convertOldPath(assetAction.oldSrc);

                    await this.assetEditorService.addImage(deck.id, deck.data.owner_id, assetAction.newPath, oldPath);
                }

                resolve();
            });
        });
    }

    private convertOldPath(oldSrc: string): Promise<string | undefined> {
        return new Promise<string | undefined>((resolve) => {
            if (!oldSrc || oldSrc === undefined || oldSrc.length <= 0) {
                resolve(undefined);
                return;
            }

            const storageUrl: string = EnvironmentConfigService.getInstance().get('firebase').storageUrl;
            const oldPath: string | undefined =
                decodeURIComponent(oldSrc.replace(storageUrl, '').replace(`?${Resources.Constants.STORAGE.MEDIA_PARAM}`, ''));

            resolve(oldPath);
        });
    }
}
