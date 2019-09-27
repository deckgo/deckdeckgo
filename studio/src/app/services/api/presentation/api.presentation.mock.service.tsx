import {ApiPresentationService} from './api.presentation.service';
import {ApiDeck} from '../../../models/api/api.deck';
import {ApiPresentation} from '../../../models/api/api.presentation';

export class ApiPresentationMockService extends ApiPresentationService {

    // @Override
    protected query(_deck: ApiDeck, _context: string, _method: string, _bearer?: string): Promise<ApiPresentation> {
        return new Promise<ApiPresentation>(async (resolve) => {
            resolve({
                id: Math.random().toString(36).substring(2) + Date.now().toString(36),
                url: 'https://beta.deckdeckgo.io/daviddalbusco/introducing-deckdeckgo/'
            });
        });
    }

}
