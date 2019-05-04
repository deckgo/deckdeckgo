export enum SlideTemplate {
    TITLE = 'title',
    CONTENT = 'content',
    SPLIT = 'split',
    GIF = 'gif'
}

export interface SlideAttributes {
    style?: string;
    src?: string;
    customBackground?: boolean;
}

export interface Slide {
    id?: string;
    content?: string;
    template: SlideTemplate,
    attributes?: SlideAttributes;
}
