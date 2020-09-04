import {SlideAttributes, SlideTemplate} from '../slide';

export interface ApiSlide {
  content?: string;
  template: SlideTemplate;
  attributes?: SlideAttributes;
}
