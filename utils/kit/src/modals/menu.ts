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
    result +=
      '<ion-item ion-item button detail="false" id="notes" color="primary" style="--border-style: none;"><ion-icon src="{{DECKDECKGO_BASE_HREF}}assets/icons/ionicons/clipboard.svg" aria-label="Display slide notes" slot="end"></ion-icon><ion-label>Display slide notes</ion-label></ion-item>';
  }

  result +=
    '<ion-item ion-item button detail="false" id="share" color="primary" style="--border-style: none;"><ion-icon src="{{DECKDECKGO_BASE_HREF}}assets/icons/ionicons/share.svg" aria-label="Share this presentation" slot="end"></ion-icon><ion-label>Share</ion-label></ion-item>';
  result +=
    '<ion-item ion-item button detail="false" id="made" style="--border-style: none; --ion-item-background: white;"><ion-icon src="{{DECKDECKGO_BASE_HREF}}assets/icons/deckdeckgo.svg" aria-label="DeckDeckGo" slot="end"></ion-icon><ion-label>Made with DeckDeckGo</ion-label></ion-item>';

  return result;
}

customElements.define('menu-list', MenuList);

async function openMenu($event: UIEvent) {
  $event.preventDefault();

  const popover: HTMLIonPopoverElement = document.createElement('ion-popover');
  popover.component = 'menu-list';
  popover.translucent = true;
  popover.event = $event;
  popover.mode = 'ios';
  popover.cssClass = 'options';

  document.body.appendChild(popover);

  await popover.present();
}

async function openLink(link: string) {
  window.open(link, '_blank');
  await ((document.querySelector('ion-popover') as HTMLIonPopoverElement) as HTMLIonPopoverElement).dismiss();
}

async function openShare() {
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
    url: shareUrl,
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
          socialSharePopupHeight: 400,
        },
      },
      {
        reddit: {
          socialShareUrl: shareUrl,
          socialSharePopupWidth: 300,
          socialSharePopupHeight: 500,
        },
      },
      {
        linkedin: {
          socialShareUrl: shareUrl,
        },
      },
      {
        email: {
          socialShareBody: shareUrl,
        },
      },
      {
        whatsapp: {
          socialShareUrl: shareUrl,
        },
      },
      {
        hackernews: {
          socialShareUrl: shareUrl,
        },
      },
    ],
  };

  webSocialShare.share = share;

  webSocialShare.show = true;
}
