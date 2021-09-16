import {Slide} from '@deckdeckgo/editor';

export class SlideFirebaseProvider {
  private static instance: SlideFirebaseProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SlideFirebaseProvider.instance) {
      SlideFirebaseProvider.instance = new SlideFirebaseProvider();
    }
    return SlideFirebaseProvider.instance;
  }

  async update(deckId: string, slide: Slide): Promise<void> {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {updateSlide} = await import(cdn);

    return updateSlide(deckId, slide);
  }

  async delete(deckId: string, slideId: string): Promise<void> {
    const cdn: string = 'http://localhost:3335/build/index.esm.js';

    const {deleteSlide} = await import(cdn);

    return deleteSlide(deckId, slideId);
  }
}
