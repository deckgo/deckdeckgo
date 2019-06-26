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

export interface SlideData {
    content?: string;
    template: SlideTemplate,
    attributes?: SlideAttributes;

    created_at?: firebase.firestore.Timestamp;
    updated_at?: firebase.firestore.Timestamp;
}

export interface Slide {
    id: string;
    data: SlideData;
}
