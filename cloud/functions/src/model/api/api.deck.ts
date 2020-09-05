import {DeckAttributes} from '../data/deck';
import {ApiSlide} from './api.slide';

export interface ApiDeck {
  id?: string;
  slides: ApiSlide[];
  name: string;
  description: string;
  owner_id: string;
  attributes?: DeckAttributes;
  background?: string;
  header?: string;
  footer?: string;
  head_extra?: string;
}
