import {Slide} from '../../../models/slide';

import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import {AuthService} from '../../data/auth/auth.service';

export class ApiSlideService {

    private static instance: ApiSlideService;

    private authService: AuthService;

    private constructor() {
        // Private constructor, singleton
        this.authService = AuthService.getInstance();
    }

    static getInstance() {
        if (!ApiSlideService.instance) {
            ApiSlideService.instance = new ApiSlideService();
        }
        return ApiSlideService.instance;
    }


    post(deckId: string, slide: Slide): Promise<Slide> {
        return this.query(slide, `/decks/${deckId}/slides`, 'POST');
    }

    put(deckId: string, slide: Slide): Promise<Slide> {
        return this.query(slide, `/decks/${deckId}/slides/${slide.id}`,'PUT');
    }

    delete(deckId: string, slide_id: string): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + `/decks/${deckId}/slides/` + slide_id, {
                    method: 'DELETE',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': await this.authService.getBearer()
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while deleting the slide');
                    return;
                }

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }

    private query(slide: Slide, context: string, method: string): Promise<Slide> {
        return new Promise<Slide>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + context, {
                    method: method,
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': await this.authService.getBearer()
                    },
                    body: JSON.stringify(slide)
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while creating or updating the slide');
                    return;
                }

                const persistedSlide: Slide = await rawResponse.json();

                resolve(persistedSlide);
            } catch (err) {
                reject(err);
            }
        });
    }

    get(deckId: string, slideId: string): Promise<Slide> {
        return new Promise<Slide>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + `/decks/${deckId}/slides/${slideId}`, {
                    method: 'GET',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json',
                        'Authorization': await this.authService.getBearer()
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while loading the slide');
                    return;
                }

                const slide: Slide = await rawResponse.json();

                resolve(slide);
            } catch (err) {
                reject(err);
            }
        });
    }
}
