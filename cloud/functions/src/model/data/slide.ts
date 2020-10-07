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

export enum SlideChartType {
  LINE = 'line',
  PIE = 'pie',
  BAR = 'bar',
}

export enum SlideSplitType {
  DEFAULT = 'default',
  DEMO = 'demo',
}

export type SlideAttributesYAxisDomain = 'max' | 'extent';

export interface SlideAttributes {
  style?: string;
  src?: string;
  customBackground?: string;
  imgSrc?: string;
  imgAlt?: string;

  content?: string;
  customQRCode?: boolean;

  type?: SlideChartType | SlideSplitType;
  innerRadius?: number;
  animation?: boolean;
  datePattern?: string;
  yAxisDomain?: SlideAttributesYAxisDomain;
  smooth?: boolean;
  area?: boolean;
  ticks?: number;
  grid?: boolean;
  separator?: string;

  vertical?: boolean;

  imgMode?: string;

  customLoader?: boolean;

  theme?: string;
}

export interface SlideData {
  content?: string;
  template: SlideTemplate;
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
