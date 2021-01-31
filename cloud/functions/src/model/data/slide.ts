import {firestore} from 'firebase-admin';

export enum SlideTemplate {
  TITLE = 'title',
  CONTENT = 'content',
  SPLIT = 'split',
  GIF = 'gif',
  AUTHOR = 'author',
  YOUTUBE = 'youtube',
  QRCODE = 'qrcode',
  CHART = 'chart',
  POLL = 'poll',
  'ASPECT-RATIO' = 'aspect-ratio',
  PLAYGROUND = 'playground',
}

export enum SlideScope {
  DEFAULT = 'default',
  COMMUNITY = 'community',
  USER = 'user',
}

export interface SlideAttributes {
  [key: string]: string | number | boolean;
}

export interface SlideData {
  content?: string;

  template: SlideTemplate | string;
  scope?: SlideScope;

  attributes?: SlideAttributes;

  api_id?: string;

  created_at?: firestore.Timestamp;
  updated_at?: firestore.Timestamp;
}

export interface Slide {
  id: string;
  ref: firestore.DocumentReference;
  data: SlideData;
}
