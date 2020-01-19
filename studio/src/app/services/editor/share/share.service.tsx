import {take} from 'rxjs/operators';

import {EnvironmentDeckDeckGoConfig} from '../../core/environment/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import {Deck} from '../../../models/data/deck';
import {AuthUser} from '../../../models/auth/auth.user';

import {DeckEditorService} from '../deck/deck-editor.service';
import {AuthService} from '../../auth/auth.service';

export class ShareService {

    private static instance: ShareService;

    private deckEditorService: DeckEditorService;
    private authService: AuthService;

    private constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
        this.authService = AuthService.getInstance();
    }

    static getInstance() {
        if (!ShareService.instance) {
            ShareService.instance = new ShareService();
        }
        return ShareService.instance;
    }

    getPublishedUrl(): Promise<string> {
        return new Promise<string>((resolve) => {
            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                if (deck && deck.data && deck.data.meta && deck.data.meta.pathname && deck.data.meta.pathname !== '') {
                    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
                    resolve(config.presentationUrl + deck.data.meta.pathname);
                } else {
                    // Should not happens
                    const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
                    resolve(deckDeckGoConfig.appUrl);
                }
            });
        });
    }

    getShareText(): Promise<string> {
        return new Promise<string>((resolve) => {
            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                if (deck && deck.data && deck.data.name && deck.data.name !== '') {
                    this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
                        if (authUser && !authUser.anonymous && authUser.name && authUser.name !== '') {
                            resolve(`"${deck.data.name}" by ${authUser.name} created with DeckDeckGo`);
                        } else {
                            resolve(`"${deck.data.name}" created with DeckDeckGo`);
                        }
                    });
                } else {
                    resolve('A presentation created with DeckDeckGo');
                }
            });
        });
    }
}
