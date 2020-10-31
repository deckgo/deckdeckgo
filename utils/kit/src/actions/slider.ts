export const previousSlide = async () => {
  const deck: HTMLDeckgoDeckElement | null = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!deck) {
    return;
  }

  if (window.event) {
    window.event.stopPropagation();
  }

  await deck.slidePrev();
};

export const nextSlide = async () => {
  const deck: HTMLDeckgoDeckElement | null = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!deck) {
    return;
  }

  if (window.event) {
    window.event.stopPropagation();
  }

  await deck.slideNext();
};

export const firstSlide = async () => {
  const deck: HTMLDeckgoDeckElement | null = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!deck) {
    return;
  }

  await deck.slideTo(0, 2000);
};

export const toggleFullScreen = async () => {
  const deck: HTMLDeckgoDeckElement | null = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!deck) {
    return;
  }

  await deck.toggleFullScreen();
};
