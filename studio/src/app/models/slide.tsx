export enum SlideTemplate {
    TITLE = 'title',
    CONTENT = 'content',
    SPLIT = 'split',
    GIF = 'gif'
}

export interface SlideAttributes {
    style?: string;
    src?: string;
    customBackground?: string;
}

export interface Slide {
    id?: string;
    content?: string;
    template: SlideTemplate,
    attributes?: SlideAttributes;
}
