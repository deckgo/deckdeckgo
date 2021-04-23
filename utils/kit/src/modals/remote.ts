export const openRemote = async ($event: UIEvent | undefined) => {
  $event?.preventDefault();

  // See webcomponents service ConnectionState for values
  const connected = REMOTE_STATE > 0 && REMOTE_STATE < 4;

  if (connected && PENDING_REMOTE_REQUESTS && PENDING_REMOTE_REQUESTS.length > 0) {
    const popover = document.querySelector('ion-popover.access') as HTMLIonPopoverElement;

    if (!popover) {
      await closeRemote();
      await askAccess();
    }
  } else {
    await displayRemote($event, connected);
  }
};

async function displayRemote($event: UIEvent | undefined, connected: boolean) {
  const element = document.createElement('div');
  element.innerHTML = `<div class="ion-padding">
    ${getRemoteControlInfo(connected)}
  </div>
  `;

  const popover = document.createElement('ion-popover');
  popover.component = element;

  if ($event) {
    popover.event = $event;
  }

  popover.mode = 'ios';
  popover.cssClass = 'info';

  document.body.appendChild(popover);

  await popover.present();

  const toggleElement = document.querySelector('ion-popover.info ion-toggle');

  if (toggleElement) {
    toggleElement.addEventListener('ionChange', ($event) => {
      handleRemoteControlState($event as CustomEvent);
    });
  }
}

async function closeRemote() {
  const popover = document.querySelector('ion-popover');

  if (popover) {
    await popover.dismiss();
  }
}

async function askAccess() {
  const element = document.createElement('div');
  element.innerHTML = `<div class="ion-padding">
    ${getRemoteControlRequest()}
  </div>
  `;

  const popover = document.createElement('ion-popover');
  popover.component = element;

  popover.mode = 'ios';
  popover.cssClass = 'access';

  document.body.appendChild(popover);

  await popover.present();

  // listen for close event
  const buttonDismiss: HTMLButtonElement | null = document.querySelector('ion-popover.access button.dismiss');
  buttonDismiss?.addEventListener('click', async () => {
    await removeFirstPendingRequest();

    await ((document.querySelector('ion-popover.access') as HTMLIonPopoverElement) as HTMLIonPopoverElement).dismiss();

    await nextPendingRequests();
  });

  const buttonConnect: HTMLButtonElement | null = document.querySelector('ion-popover.access button.connect');
  buttonConnect?.addEventListener('click', async () => {
    await grantRemoteConnection();

    await (document.querySelector('ion-popover.access') as HTMLIonPopoverElement).dismiss();

    await nextPendingRequests();
  });
}

async function handleRemoteControlState($event: CustomEvent) {
  if (!$event || !$event.detail) {
    return;
  }

  if ($event.detail.checked) {
    await reconnectRemoteControl();
  } else {
    await disconnectRemoteControl();
  }
}

function getRemoteControlInfo(connected: boolean) {
  if (process.env.NO_REMOTE) {
    return `<p style="margin: 0; font-size: 16px;">No remote control support available for this presentation.</p>`;
  } else {
    const remoteRoom = getRemoteControlRoom();
    const qrCodeLink = `https://deckdeckgo.app/${remoteRoom && remoteRoom !== '' ? `remote/${remoteRoom}` : ''}`;

    return `<p style="margin: 0; font-size: 16px;">Remote control your presentation with a phone or any devices.</p>
  
    <p style="margin: 0; font-size: 16px;" class="ion-padding-top">Scan the QR-Code or get the Progressive Web Apps at <a href="https://deckdeckgo.app" target="_blank" style="color: var(--ion-color-tertiary);">https://deckdeckgo.app <ion-icon name="open" style="color: var(--ion-color-tertiary); vertical-align: bottom;"></ion-icon></a></p>

    <div class="qrcode-container" style="display: flex; justify-content: center;  --deckgo-qrcode-size: 316px;">
        <deckgo-qrcode content="${qrCodeLink}">
            <ion-icon slot="logo" src="https://deckdeckgo.com/assets/icons/deckdeckgo.svg"></ion-icon>
        </deckgo-qrcode>
    </div>
    
    <ion-list class="ion-no-margin">
        <ion-item style="--border-color: transparent; color: black; --padding-start: 0; --inner-padding-end: 0;">
            <ion-label class="ion-text-wrap" style="margin: 0; font-size: 16px;">Remote control on/off</ion-label> 
            <ion-toggle mode="md" slot="end" checked="${
              connected ? 'true' : 'false'
            }" color="primary" style="--background: var(--ion-color-light); --handle-background: white; --handle-background-checked: white;"></ion-toggle>
        </ion-item>
    </ion-list>

    <p class="ion-no-margin" style="padding-top: 8px; margin-bottom: 0; font-size: 12px;">If you can't connect or loose the connection, toggle off and on the remote to restart.</p>

    <p class="ion-no-margin ion-padding-top" style="margin-bottom: 0; font-size: 12px;">Your presentation's: ${remoteRoom}</p>`;
  }
}

function getRemoteControlRoom() {
  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement) {
    return '';
  }

  return deckgoRemoteElement.room ? deckgoRemoteElement.room : '';
}

function getRemoteControlRequest() {
  if (!PENDING_REMOTE_REQUESTS || PENDING_REMOTE_REQUESTS.length <= 0) {
    return `<p style="margin: 0; font-size: 16px;">No pending requests.</p>`;
  }

  const request = PENDING_REMOTE_REQUESTS[0];

  return `<p style="margin: 0; font-size: 16px;">Grant access to remote <strong>${request.message}</strong>?</p>

    <div class="actions" style="display: flex; justify-content: center; align-content: center;">
        <button class="navigation ion-activatable transparent dismiss">
          <ion-ripple-effect></ion-ripple-effect>
          <ion-icon src="https://deckdeckgo.com/assets/icons/ionicons/close.svg"></ion-icon>
        </button>
        
        <button class="navigation ion-activatable primary connect">
          <ion-ripple-effect></ion-ripple-effect>
          <ion-icon src="https://deckdeckgo.com/assets/icons/ionicons/checkmark.svg"></ion-icon>
        </button>
    </div>
    `;
}

async function removeFirstPendingRequest() {
  if (!PENDING_REMOTE_REQUESTS || PENDING_REMOTE_REQUESTS.length <= 0) {
    return;
  }

  PENDING_REMOTE_REQUESTS.shift();
}

async function grantRemoteConnection() {
  if (!PENDING_REMOTE_REQUESTS || PENDING_REMOTE_REQUESTS.length <= 0) {
    return;
  }

  const deckgoRemoteElement = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (deckgoRemoteElement) {
    await deckgoRemoteElement.start(PENDING_REMOTE_REQUESTS[0].fromSocketId);
  }

  await removeFirstPendingRequest();
}

async function nextPendingRequests() {
  if (!PENDING_REMOTE_REQUESTS || PENDING_REMOTE_REQUESTS.length <= 0) {
    return;
  }

  // If we are connected no need to browse next pending requests
  if (REMOTE_STATE === 3) {
    return;
  }

  // If we get some more requests pending, we automatically reopen the popover
  await askAccess();
}

export async function disconnectRemoteControl() {
  const deckgoRemoteElement: HTMLDeckgoRemoteElement | null = document.querySelector('deckgo-remote') as HTMLDeckgoRemoteElement | null;

  if (!deckgoRemoteElement) {
    return;
  }

  await deckgoRemoteElement.disconnect();
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
