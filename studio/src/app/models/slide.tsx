import {SlideTemplate} from './slide-template';
import {SlideAttributes} from './slide-attributes';

export interface Slide {
    id?: string;
    content?: string;
    template: SlideTemplate,
    attributes?: SlideAttributes;
}
