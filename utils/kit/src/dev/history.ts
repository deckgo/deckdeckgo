const keepHistory = process.env.KEEP_HISTORY;

export const postLoadingJumpTo = (): Promise<void> => {
  return new Promise<void>(async (resolve) => {
    if (!keepHistory) {
      resolve();
      return;
    }

    const slider = document.getElementById('slider');

    if (slider) {
      slider.addEventListener('slidesDidLoad', async (_slides) => {
        await jumpToSlideIndexWithUrl(0);
      });
    }

    resolve();
  });
};

export const initDeckHistoryWatch = (): Promise<void> => {
  return new Promise<void>(async (resolve) => {
    if (!keepHistory) {
      resolve();
      return;
    }

    const slider = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

    if (slider) {
      slider.addEventListener('slideNextDidChange', async () => {
        await pushStateSlideIndex(slider);
      });

      slider.addEventListener('slidePrevDidChange', async () => {
        await pushStateSlideIndex(slider);
      });

      slider.addEventListener('slideToChange', async (event) => {
        await pushStateSlideIndex(slider);
      });
    }

    if (window) {
      window.onpopstate = async ($event: PopStateEvent) => {
        await jumpToSlideIndexWithUrl(300);
      };
    }

    resolve();
  });
};

export async function pushStateSlideIndex(slider: HTMLDeckgoDeckElement) {
  if (!history || !keepHistory) {
    return;
  }

  const index = await slider.getActiveIndex();

  const url = new URL(window.location.href);
  const urlIndex = url && url.searchParams ? url.searchParams.get('index') : '-1';

  if ((urlIndex === null || parseInt(urlIndex, 0) !== index) && index >= 0) {
    history.pushState({slideIndex: index}, '', index > 0 ? '?index=' + index : '');
  }
}

async function jumpToSlideIndexWithUrl(speed: number) {
  if (!keepHistory) {
    return;
  }

  const url = new URL(window.location.href);
  const index = url && url.searchParams ? url.searchParams.get('index') : '-1';

  if (index === null || parseInt(index, 0) >= 0) {
    const slider: HTMLDeckgoDeckElement | null = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

    if (!slider) {
      return;
    }

    await slider.slideTo(index === null ? 0 : parseInt(index, 0), speed);
  }
}
