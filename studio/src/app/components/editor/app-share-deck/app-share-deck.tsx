import {Component, Element, h, Method} from '@stencil/core';

import 'web-social-share';

import {ShareService} from '../../../services/editor/share/share.service';

@Component({
  tag: 'app-share-deck',
  styleUrl: 'app-share-deck.scss',
  shadow: true
})
export class AppShareDeck {
  @Element() el: HTMLElement;

  private shareService: ShareService;

  constructor() {
    this.shareService = ShareService.getInstance();
  }

  @Method()
  openShare(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      // @ts-ignore
      if (navigator && navigator.share) {
        await this.shareMobile();
      } else {
        await this.shareDesktop();
      }

      resolve();
    });
  }

  private shareMobile() {
    return new Promise(async (resolve) => {
      const text: string = await this.shareService.getShareText();
      const publishedUrl: string = await this.shareService.getPublishedUrl();

      // @ts-ignore
      await navigator.share({
        text: text,
        url: publishedUrl
      });

      resolve();
    });
  }

  private shareDesktop() {
    return new Promise(async (resolve) => {
      const webSocialShare = this.el.shadowRoot.querySelector('web-social-share');

      if (!webSocialShare || !window) {
        return;
      }

      const publishedUrl: string = await this.shareService.getPublishedUrl();

      const shareOptions = {
        displayNames: true,
        config: [
          {
            twitter: {
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
              socialShareBody: publishedUrl
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
          }
        ]
      };

      webSocialShare.share = shareOptions;

      webSocialShare.show = true;

      resolve();
    });
  }

  render() {
    return (
      <web-social-share show={false}>
        <ion-icon name="logo-twitter" slot="twitter" style={{color: '#00aced', 'font-size': '1.6rem', display: 'block'}}></ion-icon>
        <ion-icon name="logo-linkedin" slot="linkedin" style={{color: '#0077b5', 'font-size': '1.6rem', display: 'block'}}></ion-icon>
        <ion-icon name="mail" slot="email" style={{color: 'var(--ion-color-tertiary)', 'font-size': '1.6rem', display: 'block'}}></ion-icon>
        <ion-icon name="logo-whatsapp" slot="whatsapp" style={{color: '#25D366', 'font-size': '1.6rem', display: 'block'}}></ion-icon>
        <ion-icon name="copy" slot="copy" style={{'font-size': '1.6rem', display: 'block'}}></ion-icon>
      </web-social-share>
    );
  }
}
