import {displaySlideNotes} from '../modals/notes';
import {toggleFullScreen} from '../actions/slider';
import {isMobile} from '@deckdeckgo/utils';
import {openRemote} from '../modals/remote';

class MenuList extends HTMLElement {
  constructor() {
    super();
  }

  async connectedCallback() {
    const menuListActions = await buildMenuListActions();

    this.innerHTML = '<ion-list no-margin>' + menuListActions + '</ion-list>';
  }
}

async function buildMenuListActions(): Promise<string> {
  let result = '';

  if (!EMBEDDED) {
    const mobile = isMobile();

    if (!mobile) {
      result +=
        '<ion-item ion-item button detail="false" id="fullScreenMenu" style="--border-style: none;"><ion-icon src="https://deckdeckgo.com/assets/icons/ionicons/expand.svg" aria-label="Enter full screen" slot="end"></ion-icon><ion-label>Full screen</ion-label></ion-item>';
    }

    result +=
      '<ion-item ion-item button detail="false" id="remoteMenu" style="--border-style: none;"><ion-icon src="https://deckdeckgo.com/assets/icons/ionicons/phone-portrait.svg" aria-label="Remote control" slot="end"></ion-icon><ion-label>Remote control</ion-label></ion-item>';

    result +=
      '<ion-item ion-item button detail="false" id="notes" style="--border-style: none;"><ion-icon src="https://deckdeckgo.com/assets/icons/ionicons/clipboard.svg" aria-label="Display slide notes" slot="end"></ion-icon><ion-label>Slide notes</ion-label></ion-item>';
  }

  result +=
    '<ion-item ion-item button detail="false" id="share" style="--border-style: none;"><ion-icon src="https://deckdeckgo.com/assets/icons/ionicons/share.svg" aria-label="Share this presentation" slot="end"></ion-icon><ion-label>Share</ion-label></ion-item>';
  result +=
    '<ion-item ion-item button detail="false" id="made" style="--border-style: none; --ion-item-background: white;"><ion-icon src="https://deckdeckgo.com/assets/icons/deckdeckgo.svg" aria-label="DeckDeckGo" slot="end"></ion-icon><ion-label>Made with DeckDeckGo</ion-label></ion-item>';

  return result;
}

customElements.define('menu-list', MenuList);

export async function openMenu($event: UIEvent) {
  $event.preventDefault();

  const popover: HTMLIonPopoverElement = document.createElement('ion-popover');
  popover.component = 'menu-list';
  popover.translucent = true;
  popover.event = $event;
  popover.mode = 'ios';
  popover.cssClass = 'options';

  document.body.appendChild(popover);

  await popover.present();

  // attach events
  const buttonFullscreen: HTMLButtonElement | null = document.querySelector('ion-popover #fullScreenMenu');
  buttonFullscreen?.addEventListener('click', async () => {
    await fullScreenAndClose();
  });

  const buttonRemote: HTMLButtonElement | null = document.querySelector('ion-popover #remoteMenu');
  buttonRemote?.addEventListener('click', async () => {
    await openRemoteAndClose($event);
  });

  const buttonNotes: HTMLButtonElement | null = document.querySelector('ion-popover #notes');
  buttonNotes?.addEventListener('click', displaySlideNotes);

  const buttonShare: HTMLButtonElement | null = document.querySelector('ion-popover #share');
  buttonShare?.addEventListener('click', openShare);

  const buttonMade: HTMLButtonElement | null = document.querySelector('ion-popover #made');
  buttonMade?.addEventListener('click', async () => {
    await openLink('https://deckdeckgo.com');
  });
}

async function openRemoteAndClose($event: UIEvent) {
  await (document.querySelector('ion-popover') as HTMLIonPopoverElement as HTMLIonPopoverElement).dismiss();
  await openRemote($event);
}

async function fullScreenAndClose() {
  await (document.querySelector('ion-popover') as HTMLIonPopoverElement as HTMLIonPopoverElement).dismiss();
  await toggleFullScreen();
}

async function openLink(link: string) {
  window.open(link, '_blank');
  await (document.querySelector('ion-popover') as HTMLIonPopoverElement as HTMLIonPopoverElement).dismiss();
}

async function openShare() {
  // @ts-ignore
  if (navigator && navigator.share) {
    await shareMobile();
  } else {
    await shareDesktop();
  }

  await (document.querySelector('ion-popover') as HTMLIonPopoverElement).dismiss();
}

async function shareMobile() {
  const shareUrl = `${window.location.protocol}//${window.location.host}${window.location.pathname}`;

  await navigator.share({
    title: document.title,
    url: shareUrl
  });
}

async function shareDesktop() {
  const webSocialShare = document.querySelector('web-social-share');

  if (!webSocialShare || !window) {
    return;
  }

  const shareUrl = `${window.location.protocol}//${window.location.host}${window.location.pathname}`;

  const share = {
    displayNames: true,
    config: [
      {
        twitter: {
          socialShareUrl: shareUrl,
          socialSharePopupWidth: 300,
          socialSharePopupHeight: 400
        }
      },
      {
        reddit: {
          socialShareUrl: shareUrl,
          socialSharePopupWidth: 300,
          socialSharePopupHeight: 500
        }
      },
      {
        linkedin: {
          socialShareUrl: shareUrl
        }
      },
      {
        email: {
          socialShareBody: shareUrl
        }
      },
      {
        whatsapp: {
          socialShareUrl: shareUrl
        }
      },
      {
        hackernews: {
          socialShareUrl: shareUrl
        }
      }
    ]
  };

  webSocialShare.share = share;

  webSocialShare.show = true;
}
