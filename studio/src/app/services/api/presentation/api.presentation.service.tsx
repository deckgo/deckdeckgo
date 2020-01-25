import {ApiDeck} from '../../../models/api/api.deck';

import {ApiPresentation} from '../../../models/api/api.presentation';

import {AuthService} from '../../auth/auth.service';

export abstract class ApiPresentationService {
  protected authService: AuthService;

  public constructor() {
    // Private constructor, singleton
    this.authService = AuthService.getInstance();
  }

  protected abstract query(deck: ApiDeck, context: string, method: string, bearer?: string): Promise<ApiPresentation>;

  post(deck: ApiDeck): Promise<ApiPresentation> {
    return this.query(deck, '/presentations', 'POST');
  }

  put(apiDeckId: string, deck: ApiDeck, bearer?: string): Promise<ApiPresentation> {
    return this.query(deck, `/presentations/${apiDeckId}`, 'PUT', bearer);
  }
}
