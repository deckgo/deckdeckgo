import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import {ApiDeck} from '../../../models/api/api.deck';

import {EnvironmentDeckDeckGoConfig} from '../../core/environment/environment-config';

import {ApiPresentation} from '../../../models/api/api.presentation';

import {AuthService} from '../../auth/auth.service';

export class ApiPresentationService {

    private static instance: ApiPresentationService;

    private authService: AuthService;

    private constructor() {
        // Private constructor, singleton
        this.authService = AuthService.getInstance();
    }

    static getInstance() {
        if (!ApiPresentationService.instance) {
            ApiPresentationService.instance = new ApiPresentationService();
        }
        return ApiPresentationService.instance;
    }

    post(deck: ApiDeck): Promise<ApiPresentation> {
        return this.query(deck, '/presentations', 'POST');
    }

    put(deck: ApiDeck, bearer?: string): Promise<ApiPresentation> {
        return this.query(deck, `/presentations/${deck.id}`, 'PUT', bearer);
    }

    private query(deck: ApiDeck, context: string, method: string, bearer?: string): Promise<ApiPresentation> {
        return new Promise<ApiPresentation>(async (resolve, reject) => {
            try {
                const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

                if (!bearer) {
                    bearer = await this.authService.getBearer();
                }

                const rawResponse: Response = await fetch(config.apiUrl + context, {
                    method: method,
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': bearer
                    },
                    body: JSON.stringify(deck)
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while creating or updating the deck');
                    return;
                }

                const publishedPresentation: ApiPresentation = await rawResponse.json();

                resolve(publishedPresentation);
            } catch (err) {
                reject(err);
            }
        });
    }

}

