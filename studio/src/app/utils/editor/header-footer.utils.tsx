import {EnvironmentDeckDeckGoConfig} from '../../services/core/environment/environment-config';
import {EnvironmentConfigService} from '../../services/core/environment/environment-config.service';

import {SlotType} from './slot-type';

import {User} from '../../models/data/user';

export class HeaderFooterUtils {
  static async append(
    deck: HTMLElement,
    user: User,
    slotName: 'header' | 'footer',
    type: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom'
  ): Promise<void> {
    if (!user && !user.data && !user.data.social) {
      return;
    }

    if (!deck) {
      return;
    }

    const currentSlotElement: HTMLElement = deck.querySelector(`:scope > [slot='${slotName}']`);

    if (currentSlotElement) {
      deck.removeChild(currentSlotElement);
    }

    const promises: Promise<HTMLElement>[] = [this.createContainer(slotName), this.createSocial(user, type), this.createImg(type)];
    const [div, social, deckgoImg] = await Promise.all(promises);

    social.appendChild(deckgoImg);

    div.appendChild(social);

    deck.appendChild(div);

    // prettier-ignore
    deckgoImg.addEventListener('lazyImgDidLoad', async () => {
      if (slotName === 'footer') {
        await (deck as any).loadFooter();
      } else {
        await (deck as any).loadHeader();
      }
      
      // TODO: this.didChange.emit(deck);
    }, {once: true});
  }

  private static async createContainer(slotName: 'header' | 'footer'): Promise<HTMLDivElement> {
    const div: HTMLDivElement = document.createElement('div');
    div.setAttribute('slot', slotName);
    div.setAttribute('contentEditable', 'false');

    return div;
  }

  private static async createSocial(user: User, type: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom'): Promise<HTMLElement> {
    const socialElement = document.createElement('deckgo-social');

    socialElement.setAttribute(type === 'custom' ? 'full-url' : type, user.data.social[type]);

    return socialElement;
  }

  private static async createImg(type: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom'): Promise<HTMLElement> {
    const deckgoImg: HTMLElement = document.createElement(SlotType.IMG);
    deckgoImg.setAttribute('slot', 'icon');
    deckgoImg.setAttribute('aria-label', type);

    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    if (type === 'twitter' || type === 'linkedin' || type === 'github') {
      deckgoImg.setAttribute('svg-src', `${config.globalAssetsUrl}/icons/ionicons/${type}.svg`);
    } else if (type === 'medium' || type === 'dev') {
      deckgoImg.setAttribute('svg-src', `${config.globalAssetsUrl}/icons/${type}.svg`);
    } else {
      deckgoImg.setAttribute('svg-src', `${config.globalAssetsUrl}/icons/ionicons/globe.svg`);
    }

    return deckgoImg;
  }
}
