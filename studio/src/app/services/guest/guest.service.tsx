import {take} from 'rxjs/operators';

import {User} from '../../models/user';

import {AuthService} from '../auth/auth.service';

export class GuestService {

    private static instance: GuestService;

    private authService: AuthService;

    private constructor() {
        // Private constructor, singleton
        this.authService = AuthService.getInstance();
    }

    static getInstance() {
        if (!GuestService.instance) {
            GuestService.instance = new GuestService();
        }
        return GuestService.instance;
    }

    /**
     * We limit anonymous user to add three slides
     * @param slides
     */
    couldAddSlide(slides: any[]): Promise<boolean> {
        return new Promise<boolean>(async (resolve) => {
            if (!slides || slides.length <= 0) {
                resolve(true);
                return;
            }

            this.authService.watch().pipe(take(1)).subscribe((user: User) => {
                if (!user) {
                    resolve(false);
                    return;
                }

                if (!user.anonymous) {
                    resolve(true);
                    return;
                }

                resolve(slides.length < 3);
            });
        });
    }

    couldPublish(slides: any[]): Promise<boolean> {
        return new Promise<boolean>((resolve) => {
            if (!slides || slides.length <= 0) {
                resolve(true);
                return;
            }

            this.authService.watch().pipe(take(1)).subscribe((user: User) => {
                if (!user) {
                    resolve(false);
                    return;
                }

                resolve(!user.anonymous);
            });
        });
    }

}
