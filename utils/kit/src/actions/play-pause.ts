export const videoStart = async () => {
  return playPause('play', true);
};

export const videoPause = async () => {
  return playPause('pause', true);
};

export const playPause = (action: 'play' | 'pause', forwardToRemote: boolean): Promise<void> => {
  return new Promise(async (resolve) => {
    const deck: HTMLDeckgoDeckElement | null = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

    if (!deck) {
      resolve();
      return;
    }

    const index = await deck.getActiveIndex();

    const actionSlideElement = document.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

    if (
      !actionSlideElement ||
      (actionSlideElement.tagName !== 'deckgo-slide-youtube'.toUpperCase() &&
        actionSlideElement.tagName !== 'deckgo-slide-video'.toUpperCase())
    ) {
      resolve();
      return;
    }

    if (action === 'pause') {
      await (actionSlideElement as HTMLSlideVideoElement).pause();
    } else {
      await (actionSlideElement as HTMLSlideVideoElement).play();
    }

    if (forwardToRemote) {
      await forwardPlayPauseToRemote(action);
    }
  });
};

async function forwardPlayPauseToRemote(action: string) {
  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement) {
    return;
  }

  if (action === 'pause') {
    await deckgoRemoteElement.pause();
  } else {
    await deckgoRemoteElement.play();
  }
}
