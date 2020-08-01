import {Component, Element, h, Method} from '@stencil/core';

import 'web-social-share';

import shareStore from '../../../stores/share.store';

import {getPublishedUrl, getShareText, getShareTwitterText} from '../../../utils/core/share.utils';

@Component({
  tag: 'app-share-deck',
  styleUrl: 'app-share-deck.scss',
  shadow: true,
})
export class AppShareDeck {
  @Element() el: HTMLElement;

  @Method()
  async openShare() {
    // @ts-ignore
    if (navigator && navigator.share) {
      await this.shareMobile();
    } else {
      await this.shareDesktop();
    }
  }

  private async shareMobile() {
    const text: string = await getShareText(shareStore.state.share.deck, shareStore.state.share.userName);
    const publishedUrl: string = await getPublishedUrl(shareStore.state.share.deck);

    // @ts-ignore
    await navigator.share({
      text: text,
      url: publishedUrl,
    });
  }

  private async shareDesktop() {
    const webSocialShare = this.el.shadowRoot.querySelector('web-social-share');

    if (!webSocialShare || !window) {
      return;
    }

    const text: string = await getShareText(shareStore.state.share.deck, shareStore.state.share.userName);
    const twitterText: string = await getShareTwitterText(shareStore.state.share.deck, shareStore.state.share.userName, shareStore.state.share.userSocial);
    const publishedUrl: string = await getPublishedUrl(shareStore.state.share.deck);

    const shareOptions = {
      displayNames: true,
      config: [
        {
          twitter: {
            socialShareText: twitterText,
            socialShareUrl: publishedUrl,
            socialSharePopupWidth: 300,
            socialSharePopupHeight: 400,
          },
        },
        {
          linkedin: {
            socialShareUrl: publishedUrl,
          },
        },
        {
          email: {
            socialShareBody: `${text} ${publishedUrl}`,
          },
        },
        {
          whatsapp: {
            socialShareUrl: publishedUrl,
          },
        },
        {
          copy: {
            socialShareUrl: publishedUrl,
          },
        },
        {
          hackernews: {
            socialShareUrl: publishedUrl,
          },
        },
      ],
    };

    webSocialShare.share = shareOptions;

    webSocialShare.show = true;
  }

  private resetShare() {
    shareStore.reset();
  }

  render() {
    return (
      <web-social-share show={false} onClosed={() => this.resetShare()}>
        <ion-icon name="logo-twitter" slot="twitter" style={{color: '#00aced', 'font-size': '1.6rem', display: 'block'}}></ion-icon>
        <ion-icon name="logo-linkedin" slot="linkedin" style={{color: '#0077b5', 'font-size': '1.6rem', display: 'block'}}></ion-icon>
        <ion-icon name="mail-outline" slot="email" style={{color: 'var(--ion-color-tertiary)', 'font-size': '1.6rem', display: 'block'}}></ion-icon>
        <ion-icon name="logo-whatsapp" slot="whatsapp" style={{color: '#25D366', 'font-size': '1.6rem', display: 'block'}}></ion-icon>
        <ion-icon name="copy-outline" slot="copy" style={{'font-size': '1.6rem', display: 'block'}}></ion-icon>
        <ion-icon name="logo-hackernews" slot="hackernews" style={{color: '#ff6000', 'font-size': '1.6rem', display: 'block'}}></ion-icon>
      </web-social-share>
    );
  }
}
