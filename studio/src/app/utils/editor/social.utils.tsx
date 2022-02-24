import {User, UserSocial} from '@deckdeckgo/editor';
import {h, JSX} from '@stencil/core';
import {EnvironmentDeckDeckGoConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export class SocialUtils {
  static async createSocialLinks(user: User): Promise<JSX.IntrinsicElements[]> {
    const links = [];

    if (user?.data?.social) {
      const userSocial: UserSocial = user.data.social;

      if (userSocial.twitter && userSocial.twitter !== '' && userSocial.twitter !== undefined) {
        links.push(this.createTwitter(user));
      }

      if (userSocial.linkedin && userSocial.linkedin !== '' && userSocial.linkedin !== undefined) {
        links.push(this.createLinkedin(user));
      }

      if (userSocial.dev && userSocial.dev !== '' && userSocial.dev !== undefined) {
        links.push(this.createDev(user));
      }

      if (userSocial.medium && userSocial.medium !== '' && userSocial.medium !== undefined) {
        links.push(this.createMedium(user));
      }

      if (userSocial.github && userSocial.github !== '' && userSocial.github !== undefined) {
        links.push(this.createGitHub(user));
      }

      if (userSocial.custom && userSocial.custom !== '' && userSocial.custom !== undefined) {
        links.push(this.createCustom(user));
      }
    }

    return links;
  }

  static createCustom(user: User): JSX.IntrinsicElements {
    return (
      <deckgo-social slot={`social-link`} fullUrl={user.data.social.custom}>
        {this.createCustomLogo(user)}
      </deckgo-social>
    );
  }

  private static createCustomLogo(user: User): JSX.IntrinsicElements {
    if (user.data.social.custom_logo_url) {
      return <deckgo-lazy-img slot="icon" img-src={user.data.social.custom_logo_url} aria-label="Web"></deckgo-lazy-img>;
    }

    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    return <deckgo-lazy-img slot="icon" svg-src={`${config.globalAssetsUrl}/icons/ionicons/globe.svg`} aria-label="Web"></deckgo-lazy-img>;
  }

  static createGitHub(user: User): JSX.IntrinsicElements {
    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    return (
      <deckgo-social slot={`social-link`} github={user.data.social.github}>
        <deckgo-lazy-img slot="icon" svg-src={`${config.globalAssetsUrl}/icons/ionicons/github.svg`} aria-label="GitHub"></deckgo-lazy-img>
      </deckgo-social>
    );
  }

  static createMedium(user: User): JSX.IntrinsicElements {
    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    return (
      <deckgo-social slot={`social-link`} medium={user.data.social.medium}>
        <deckgo-lazy-img slot="icon" svg-src={`${config.globalAssetsUrl}/icons/medium.svg`} aria-label="Medium"></deckgo-lazy-img>
      </deckgo-social>
    );
  }

  static createDev(user: User): JSX.IntrinsicElements {
    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    return (
      <deckgo-social slot={`social-link`} dev={user.data.social.dev}>
        <deckgo-lazy-img slot="icon" svg-src={`${config.globalAssetsUrl}/icons/dev.svg`} aria-label="Dev"></deckgo-lazy-img>
      </deckgo-social>
    );
  }

  static createLinkedin(user: User): JSX.IntrinsicElements {
    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    return (
      <deckgo-social slot={`social-link`} linkedin={user.data.social.linkedin}>
        <deckgo-lazy-img
          slot="icon"
          svg-src={`${config.globalAssetsUrl}/icons/ionicons/linkedin.svg`}
          aria-label="LinkedIn"
        ></deckgo-lazy-img>
      </deckgo-social>
    );
  }

  static createTwitter(user: User): JSX.IntrinsicElements {
    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    return (
      <deckgo-social slot={`social-link`} twitter={user.data.social.twitter}>
        <deckgo-lazy-img
          slot="icon"
          svg-src={`${config.globalAssetsUrl}/icons/ionicons/twitter.svg`}
          aria-label="Twitter"
        ></deckgo-lazy-img>
      </deckgo-social>
    );
  }
}
