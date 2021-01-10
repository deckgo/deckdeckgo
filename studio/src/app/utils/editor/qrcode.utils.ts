import {Deck} from '../../models/data/deck';
import {EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';
import {EnvironmentConfigService} from '../../services/core/environment/environment-config.service';

export class QRCodeUtils {
  static getPresentationUrl(deck: Deck): string {
    if (deck && deck.data && deck.data.meta && deck.data.meta.pathname && deck.data.meta.pathname !== '') {
      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      return config.presentationUrl + deck.data.meta.pathname;
    }

    const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
    return deckDeckGoConfig.appUrl;
  }
}
