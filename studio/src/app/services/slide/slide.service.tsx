import {Slide} from '../../models/slide';

import {EnvironmentConfigService} from '../environment/environment-config.service';

export class SlideService {

    private static instance: SlideService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!SlideService.instance) {
            SlideService.instance = new SlideService();
        }
        return SlideService.instance;
    }


    // TODO: get /slides/:slide_id

    post(slide: Slide): Promise<Slide> {
        return this.query(slide, '/slides', 'POST');
    }

    put(slide: Slide): Promise<Slide> {
        return this.query(slide, '/slides/' + slide.slide_id,'PUT');
    }

    delete(slide_id: string): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + '/slides/' + slide_id, {
                    method: 'DELETE',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json'
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
                        'Content-Type': 'application/json'
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

    get(slideId: string): Promise<Slide> {
        return new Promise<Slide>(async (resolve, reject) => {
            try {
                const apiUrl: string = EnvironmentConfigService.getInstance().get('apiUrl');

                const rawResponse: Response = await fetch(apiUrl + `/slides/${slideId}`, {
                    method: 'GET',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json'
                    }
                });

                if (!rawResponse || !rawResponse.ok) {
                    reject('Something went wrong while search for the deck');
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
