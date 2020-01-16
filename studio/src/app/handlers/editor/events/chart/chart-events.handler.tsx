import {take} from 'rxjs/operators';

import {AuthUser} from '../../../../models/auth/auth.user';

import {AuthService} from '../../../../services/auth/auth.service';

export class ChartEventsHandler {

    private authService: AuthService;

    constructor() {
        this.authService = AuthService.getInstance();
    }

    init(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (document) {
                document.addEventListener('chartCustomLoad', this.onCustomLoad, false);
            }

            resolve();
        });
    }

    destroy() {
        document.removeEventListener('chartCustomLoad', this.onCustomLoad, true);
    }

    private onCustomLoad = async ($event: CustomEvent) => {
        if (!$event || !$event.detail || !$event.target || !($event.target instanceof HTMLElement)) {
            return;
        }

        await this.loadChart($event);
    };

    private loadChart($event: CustomEvent<string>): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const src: string = $event.detail;

            if (!src || src === undefined) {
                resolve();
                return;
            }

            await this.loadStorageData($event.target as HTMLElement, src);

            resolve();
        });
    }

    private loadStorageData(target: HTMLElement, src: string): Promise<void> {
        return new Promise<void>((resolve) => {
            this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
                if (authUser) {
                    const bearer: string = await this.authService.getBearer();

                    try {
                        const rawResponse: Response = await fetch(src, {
                            method: 'GET',
                            headers: {
                                'Authorization': bearer
                            }
                        });

                        if (!rawResponse || !rawResponse.ok) {
                            console.error(`Chart data ${src} can not be fetched.`);
                            resolve();
                            return;
                        }

                        const content = await rawResponse.text();
                        await (target as any).postCustomLoad(content);

                        resolve();
                    } catch (err) {
                        console.error(err);
                        resolve();
                    }
                }
            });

            resolve();
        });
    }
}
