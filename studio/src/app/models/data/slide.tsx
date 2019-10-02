export enum SlideTemplate {
    TITLE = 'title',
    CONTENT = 'content',
    SPLIT = 'split',
    GIF = 'gif',
    AUTHOR = 'author',
    YOUTUBE = 'youtube',
    QRCODE = 'qrcode'
}

export interface SlideAttributes {
    style?: string;
    src?: string;
    customBackground?: string;
    imgSrc?: string;
    imgAlt?: string;
    content?: string;
}

export interface SlideData {
    content?: string;
    template: SlideTemplate,
    attributes?: SlideAttributes;

    api_id?: string;

    created_at?: firebase.firestore.Timestamp;
    updated_at?: firebase.firestore.Timestamp;
}

export interface Slide {
    id: string;
    data: SlideData;
}
