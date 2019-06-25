export enum ApiSlideTemplate {
    TITLE = 'title',
    CONTENT = 'content',
    SPLIT = 'split',
    GIF = 'gif'
}

export interface ApiSlideAttributes {
    style?: string;
    src?: string;
    customBackground?: string;
}

export interface ApiSlide {
    id?: string;
    content?: string;
    template: ApiSlideTemplate,
    attributes?: ApiSlideAttributes;
}
