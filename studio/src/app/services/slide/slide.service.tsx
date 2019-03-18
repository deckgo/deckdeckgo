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

    post(slide: Slide): Promise<Slide> {
        return this.postOrPut(slide, '/slides', 'POST');
    }

    put(slide: Slide): Promise<Slide> {
        return this.postOrPut(slide, '/slides/' + slide.slide_id,'PUT');
    }

    private postOrPut(slide: Slide, context: string, method: string): Promise<Slide> {
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

                const persistedSlide: Slide = await rawResponse.json();

                console.log(persistedSlide);

                resolve(persistedSlide);
            } catch (err) {
                reject(err);
            }
        });
    }
}
