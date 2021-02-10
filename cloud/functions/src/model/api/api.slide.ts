import {SlideAttributes, SlideTemplate} from '../data/slide';

export interface ApiSlide {
  content?: string;
  template: SlideTemplate | string;
  attributes?: SlideAttributes;
}
