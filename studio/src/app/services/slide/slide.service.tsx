import {Slide} from '../../models/slide';

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

    post(slide: Slide): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                const rawResponse: Response = await fetch('/somewhere', {
                    method: 'POST',
                    headers: {
                        'Accept': 'application/json',
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify(slide)
                });

                const postedSlide: Slide = await rawResponse.json();

                console.log(postedSlide);

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }
}
