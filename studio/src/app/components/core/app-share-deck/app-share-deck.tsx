import {Component, Element, h, Method} from '@stencil/core';

import 'web-social-share';

import shareStore from '../../../stores/share.store';

import {getPublishedUrl, getShareText, getShareTwitterText} from '../../../utils/core/share.utils';

import {AppIcon} from '../app-icon/app-icon';

@Component({
  tag: 'app-share-deck',
  styleUrl: 'app-share-deck.scss',
  shadow: true
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
      url: publishedUrl
    });
  }

  private async shareDesktop() {
    const webSocialShare: HTMLWebSocialShareElement = this.el.shadowRoot.querySelector('web-social-share');

    if (!webSocialShare || !window) {
      return;
    }

    const text: string = await getShareText(shareStore.state.share.deck, shareStore.state.share.userName);
    const twitterText: string = await getShareTwitterText(
      shareStore.state.share.deck,
      shareStore.state.share.userName,
      shareStore.state.share.userSocial
    );
    const publishedUrl: string = await getPublishedUrl(shareStore.state.share.deck);

    const shareOptions = {
      displayNames: true,
      config: [
        {
          twitter: {
            socialShareText: twitterText,
            socialShareUrl: publishedUrl,
            socialSharePopupWidth: 300,
            socialSharePopupHeight: 400
          }
        },
        {
          linkedin: {
            socialShareUrl: publishedUrl
          }
        },
        {
          email: {
            socialShareBody: `${text} ${publishedUrl}`
          }
        },
        {
          whatsapp: {
            socialShareUrl: publishedUrl
          }
        },
        {
          copy: {
            socialShareUrl: publishedUrl
          }
        },
        {
          hackernews: {
            socialShareUrl: publishedUrl
          }
        },
        {
          telegram: {
            socialShareUrl: publishedUrl
          }
        }
      ]
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
        <AppIcon
          name="twitter"
          slot="twitter"
          ariaLabel="Twitter"
          style={{color: '#00aced', 'font-size': '1.6rem', display: 'block'}}></AppIcon>
        <AppIcon
          name="linkedin"
          slot="linkedin"
          ariaLabel="Linkedin"
          style={{color: '#0077b5', 'font-size': '1.6rem', display: 'block'}}></AppIcon>
        <AppIcon
          name="mail"
          slot="email"
          ariaLabel="Mail"
          style={{color: 'var(--ion-color-tertiary)', 'font-size': '1.6rem', display: 'block'}}></AppIcon>
        <AppIcon
          name="whatsapp"
          slot="whatsapp"
          ariaLabel="Whatsapp"
          style={{color: '#25D366', 'font-size': '1.6rem', display: 'block'}}></AppIcon>
        <AppIcon name="copy" slot="copy" ariaLabel="Copy" style={{'font-size': '1.6rem', display: 'block'}}></AppIcon>
        <AppIcon
          name="hackernews"
          slot="hackernews"
          ariaLabel="Hackernews"
          style={{color: '#ff6000', 'font-size': '1.6rem', display: 'block'}}></AppIcon>
        <AppIcon name="twitter" slot="twitter" ariaLabel="Twitter"></AppIcon>
        <svg slot="telegram" style={{color: '#0088cc', width: '1.6rem', display: 'block'}} viewBox="0 0 448 512">
          <path
            fill="currentColor"
            d="M446.7 98.6l-67.6 318.8c-5.1 22.5-18.4 28.1-37.3 17.5l-103-75.9-49.7 47.8c-5.5 5.5-10.1 10.1-20.7 10.1l7.4-104.9 190.9-172.5c8.3-7.4-1.8-11.5-12.9-4.1L117.8 284 16.2 252.2c-22.1-6.9-22.5-22.1 4.6-32.7L418.2 66.4c18.4-6.9 34.5 4.1 28.5 32.2z"></path>
        </svg>
      </web-social-share>
    );
  }
}
