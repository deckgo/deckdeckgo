import {Component, h} from '@stencil/core';
import 'web-social-share';
import {publishUrl} from '../../../providers/publish/publish.provider';
import editorStore from '../../../stores/editor.store';
import shareStore, {ShareData} from '../../../stores/share.store';
import {getShareText, getShareTwitterText} from '../../../utils/core/share.utils';
import {AppIcon} from '../app-icon/app-icon';

@Component({
  tag: 'app-share',
  styleUrl: 'app-share.scss',
  shadow: true
})
export class AppShare {
  private webSocialShareRef!: HTMLWebSocialShareElement;

  private destroyListener;

  componentWillLoad() {
    this.destroyListener = shareStore.onChange('share', async (share: ShareData | null) => await this.openShare(share));
  }

  disconnectedCallback() {
    this.destroyListener?.();
  }

  private async openShare(share: ShareData | null) {
    if (!share) {
      return;
    }

    // @ts-ignore
    if (navigator && navigator.share) {
      await this.shareMobile();
    } else {
      await this.shareDesktop();
    }
  }

  private async shareMobile() {
    const text: string = getShareText();
    const publishedUrl: string = await publishUrl(editorStore.state.doc?.data.meta || editorStore.state.deck?.data.meta);

    // @ts-ignore
    await navigator.share({
      text: text,
      url: publishedUrl
    });
  }

  private async shareDesktop() {
    if (!this.webSocialShareRef) {
      return;
    }

    const text: string = getShareText();
    const twitterText: string = getShareTwitterText();
    const publishedUrl: string = await publishUrl(editorStore.state.doc?.data.meta || editorStore.state.deck?.data.meta);

    this.webSocialShareRef.share = {
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

    this.webSocialShareRef.show = true;
  }

  private resetShare() {
    shareStore.reset();
  }

  render() {
    return (
      <web-social-share
        show={false}
        onClosed={() => this.resetShare()}
        ref={(el) => (this.webSocialShareRef = el as HTMLWebSocialShareElement)}
      >
        <AppIcon
          name="twitter"
          slot="twitter"
          ariaLabel="Twitter"
          lazy={true}
          style={{color: '#00aced', 'font-size': '1.6rem', display: 'block'}}
        ></AppIcon>
        <AppIcon
          name="linkedin"
          slot="linkedin"
          ariaLabel="LinkedIn"
          lazy={true}
          style={{color: '#0077b5', 'font-size': '1.6rem', display: 'block'}}
        ></AppIcon>
        <AppIcon
          name="mail"
          slot="email"
          ariaLabel="Mail"
          lazy={true}
          style={{color: 'var(--ion-color-tertiary)', 'font-size': '1.6rem', display: 'block'}}
        ></AppIcon>
        <AppIcon
          name="whatsapp"
          slot="whatsapp"
          ariaLabel="Whatsapp"
          lazy={true}
          style={{color: '#25D366', 'font-size': '1.6rem', display: 'block'}}
        ></AppIcon>
        <AppIcon name="copy" slot="copy" ariaLabel="Copy" style={{'font-size': '1.6rem', display: 'block'}}></AppIcon>
        <AppIcon
          name="hackernews"
          slot="hackernews"
          ariaLabel="Hackernews"
          lazy={true}
          style={{color: '#ff6000', 'font-size': '1.6rem', display: 'block'}}
        ></AppIcon>
        <AppIcon name="twitter" slot="twitter" ariaLabel="Twitter" lazy={true}></AppIcon>
        <svg slot="telegram" style={{color: '#0088cc', width: '1.6rem', display: 'block'}} viewBox="0 0 448 512">
          <path
            fill="currentColor"
            d="M446.7 98.6l-67.6 318.8c-5.1 22.5-18.4 28.1-37.3 17.5l-103-75.9-49.7 47.8c-5.5 5.5-10.1 10.1-20.7 10.1l7.4-104.9 190.9-172.5c8.3-7.4-1.8-11.5-12.9-4.1L117.8 284 16.2 252.2c-22.1-6.9-22.5-22.1 4.6-32.7L418.2 66.4c18.4-6.9 34.5 4.1 28.5 32.2z"
          ></path>
        </svg>
      </web-social-share>
    );
  }
}
