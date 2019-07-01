import {SlideAttributes, SlideTemplate} from '../data/slide';

export interface ApiSlide {
    id?: string;
    content?: string;
    template: SlideTemplate,
    attributes?: SlideAttributes;
}
