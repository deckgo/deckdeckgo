import {SlideTemplate} from './slide-template';
import {SlideAttributes} from './slide-attributes';

export interface Slide {
    id?: string;
    slide_content?: string;
    slide_template: SlideTemplate,
    slide_attributes?: SlideAttributes;
}
