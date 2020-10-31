import {playPause} from '../actions/play-pause';
import {pushStateSlideIndex} from '../dev/history';
import {initActionPlayPause} from '../actions/actions';

PENDING_REMOTE_REQUESTS = [];
REMOTE_STATE = 0;

export const remoteEvent = async ($event: CustomEvent) => {
  if (!$event || !$event.detail) {
    return;
  }

  const slider = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!slider) {
    return;
  }

  const type = $event.detail.type;

  if (type === 'next_slide') {
    const slideAnimation = $event.detail.slideAnimation;
    await slider.slideNext(slideAnimation, false);
    await pushStateSlideIndex(slider);
    await initActionPlayPause(slider);
  } else if (type === 'prev_slide') {
    const slideAnimation = $event.detail.slideAnimation;
    await slider.slidePrev(slideAnimation, false);
    await pushStateSlideIndex(slider);
    await initActionPlayPause(slider);
  } else if (type === 'slide_action') {
    await slidePlayPause($event);
  } else if (type === 'slide_to') {
    const index = $event.detail.index;
    if (index >= 0) {
      await slider.slideTo(index, 0);
      await pushStateSlideIndex(slider);
      await initActionPlayPause(slider);
    }
  } else if (type === 'deck_request') {
    await openRemoteToGrantAccess($event.detail);
  }
};

async function openRemoteToGrantAccess(fromClient: {message: string; fromSocketId: string}) {
  const buttonPopover: HTMLButtonElement | null = document.querySelector('#remote');

  if (!buttonPopover) {
    return;
  }

  PENDING_REMOTE_REQUESTS.push(fromClient);

  buttonPopover.click();
}

export async function reconnectRemoteControl() {
  const deckgoRemoteElement: HTMLDeckgoRemoteElement | null = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement) {
    return;
  }

  await deckgoRemoteElement.connect();

  const deck: HTMLDeckgoDeckElement | null = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!deck) {
    return;
  }

  await deck.slideTo(0, 300, false);
}

export async function disconnectRemoteControl() {
  const deckgoRemoteElement: HTMLDeckgoRemoteElement | null = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement) {
    return;
  }

  await deckgoRemoteElement.disconnect();
}

export const initRemote = async () => {
  if (process.env.NO_REMOTE || EMBEDDED) {
    return;
  }

  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement || !window) {
    return;
  }

  deckgoRemoteElement.addEventListener('event', async ($event) => {
    await remoteEvent($event as CustomEvent);
  });

  deckgoRemoteElement.addEventListener('state', async ($event) => {
    window.REMOTE_STATE = $event ? ($event as CustomEvent).detail : 0;
  });

  window.addEventListener('resize', async () => {
    await initRemoteSize();
  });

  await initDeck();

  await initRemoteSize();

  await initDeckMove();
};

async function initDeck() {
  const deck = document.getElementById('slider');

  if (!deck) {
    return;
  }

  deck.addEventListener('slidesDidLoad', async ($event) => {
    await initRemoteRoomServer($event as CustomEvent);
  });
}

async function initRemoteRoomServer($event: CustomEvent) {
  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement || !document) {
    return;
  }

  deckgoRemoteElement.deck = $event.detail;

  if (!deckgoRemoteElement.room) {
    // In case the presentation is published and many users are browsing it, this enhance the change to have single id
    // Or hash or timestamp would be better, but for the time being, a random number is readable and probably enough
    const roomNumber = Math.floor(Math.random() * 999);
    deckgoRemoteElement.room = ROOM_NAME ? `${ROOM_NAME} *${roomNumber}` : `DeckDeckGo *${roomNumber}`;
  }

  // SIGNALING_SERVER is declared by Webpack, see webpack.config.js
  deckgoRemoteElement.socketUrl = process.env.SIGNALING_SERVER as string;
}

async function initDeckMove() {
  const deck = document.getElementById('slider');

  if (!deck) {
    return;
  }

  deck.addEventListener('slideNextDidChange', async () => {
    await slidePrevNext(true, false);
  });

  deck.addEventListener('slidePrevDidChange', async () => {
    await slidePrevNext(false, false);
  });

  deck.addEventListener('slideNextDidAnimate', async () => {
    await slidePrevNext(true, true);
  });

  deck.addEventListener('slidePrevDidAnimate', async () => {
    await slidePrevNext(false, true);
  });

  deck.addEventListener('slideWillChange', async ($event) => {
    await moveRemote($event as CustomEvent);
  });

  deck.addEventListener('slideDrag', async ($event) => {
    await scrollRemote($event as CustomEvent);
  });

  deck.addEventListener('slideToChange', async ($event) => {
    await slideToChange($event as CustomEvent);
  });
}

async function initRemoteSize() {
  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement) {
    return;
  }

  deckgoRemoteElement.width = window.innerWidth;
  deckgoRemoteElement.height = window.innerHeight;

  const deck = document.getElementById('slider');

  if (!deckgoRemoteElement || !deck) {
    return;
  }

  deckgoRemoteElement.length = deck.childElementCount;
}

async function slideToChange($event: CustomEvent) {
  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement || !$event) {
    return;
  }

  const slideIndex = $event.detail;

  await deckgoRemoteElement.slideTo(slideIndex, 0);
}

async function slidePrevNext(next: boolean, animation: boolean) {
  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement) {
    return;
  }

  if (next) {
    await deckgoRemoteElement.nextSlide(animation);
  } else {
    await deckgoRemoteElement.prevSlide(animation);
  }
}

async function moveRemote($event: CustomEvent) {
  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement) {
    return;
  }

  await deckgoRemoteElement.moveDraw($event.detail, '300ms');
}

async function scrollRemote($event: CustomEvent) {
  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement) {
    return;
  }

  await deckgoRemoteElement.moveDraw($event.detail, '0ms');
}

function slidePlayPause($event: CustomEvent): Promise<void> {
  return playPause($event.detail.action, false);
}
