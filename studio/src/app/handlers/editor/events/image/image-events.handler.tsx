import {take} from 'rxjs/operators';

import {DeckDeckGoCustomLoad} from '@deckdeckgo/lazy-img';

import {AuthUser} from '../../../../models/auth/auth.user';

import {AuthService} from '../../../../services/auth/auth.service';
import {EnvironmentConfigService} from '../../../../services/core/environment/environment-config.service';

export class ImageEventsHandler {

    private authService: AuthService;

    private storageUrl: string = EnvironmentConfigService.getInstance().get('firebase').storageUrl;

    constructor() {
        this.authService = AuthService.getInstance();
    }

    init(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (document) {
                document.addEventListener('customLoad', this.onCustomLoad, false);
            }

            resolve();
        });
    }

    destroy() {
        document.removeEventListener('customLoad', this.onCustomLoad, true);
    }

    private onCustomLoad = async ($event: CustomEvent) => {
        if (!$event || !$event.detail || !$event.target || !($event.target instanceof HTMLElement)) {
            return;
        }

        await this.loadImage($event);
    };

    private loadImage($event: CustomEvent<DeckDeckGoCustomLoad>): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const detail: DeckDeckGoCustomLoad = $event.detail;

            if (!detail.imgSrc || detail.imgSrc === undefined) {
                resolve();
                return;
            }

            if (detail.imgSrc.indexOf(this.storageUrl) > -1) {
                await this.loadStorageImage(detail);
            } else {
                await this.loadThirdPartyImage(detail);
            }

            resolve();
        });
    }

    private loadThirdPartyImage(detail: DeckDeckGoCustomLoad): Promise<void> {
        return new Promise<void>((resolve) => {
            detail.imgElement.setAttribute('src', detail.imgSrc);

            resolve();
        });
    }

    private loadStorageImage(detail: DeckDeckGoCustomLoad): Promise<void> {
        return new Promise<void>((resolve) => {
            this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
                if (authUser) {
                    const bearer: string = await this.authService.getBearer();

                    try {
                        const rawResponse: Response = await fetch(detail.imgSrc, {
                            method: 'GET',
                            headers: {
                                'Authorization': bearer
                            }
                        });

                        if (!rawResponse || !rawResponse.ok) {
                            console.error(`Image ${detail.imgSrc} can not be fetched.`);
                            return;
                        }

                        const blob: Blob = await rawResponse.blob();
                        detail.imgElement.src = URL.createObjectURL(blob);
                    } catch (err) {
                        console.error(err);
                    }
                }
            });

            resolve();
        });
    }
}
