import authStore from '../../../stores/auth.store';

import {Constants} from '../../../types/core/constants';

export class AnonymousService {
  private static instance: AnonymousService;

  static getInstance() {
    if (!AnonymousService.instance) {
      AnonymousService.instance = new AnonymousService();
    }
    return AnonymousService.instance;
  }

  /**
   * We limit anonymous user to add three slides
   * @param slides
   */
  couldAddSlide(slidesLength: number | undefined): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      if (slidesLength === undefined) {
        resolve(true);
        return;
      }

      if (!authStore.state.authUser) {
        resolve(false);
        return;
      }

      if (!authStore.state.authUser.anonymous) {
        resolve(true);
        return;
      }

      resolve(slidesLength < Constants.DECK.MIN_SLIDES);
    });
  }

  couldPublish(slides: any[]): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!slides || slides.length <= 0) {
        resolve(false);
        return;
      }

      if (!authStore.state.authUser) {
        resolve(false);
        return;
      }

      resolve(!authStore.state.anonymous);
    });
  }
}
