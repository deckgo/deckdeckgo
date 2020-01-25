import {ApiPresentationService} from './api.presentation.service';
import {ApiDeck} from '../../../models/api/api.deck';
import {ApiPresentation} from '../../../models/api/api.presentation';

import {EnvironmentDeckDeckGoConfig} from '../../core/environment/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

export class ApiPresentationProdService extends ApiPresentationService {
  protected query(deck: ApiDeck, context: string, method: string, bearer?: string): Promise<ApiPresentation> {
    return new Promise<ApiPresentation>(async (resolve, reject) => {
      try {
        const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

        if (!bearer) {
          bearer = await this.authService.getBearer();
        }

        const rawResponse: Response = await fetch(config.apiUrl + context, {
          method: method,
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json',
            Authorization: bearer
          },
          body: JSON.stringify(deck)
        });

        if (!rawResponse || !rawResponse.ok) {
          reject('Something went wrong while creating or updating the deck');
          return;
        }

        const publishedPresentation: ApiPresentation = await rawResponse.json();

        resolve(publishedPresentation);
      } catch (err) {
        reject(err);
      }
    });
  }
}
